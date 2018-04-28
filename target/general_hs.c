#include <ir/ir.h>
#include <target/util.h>

// (NOTE: Has side effect)
// (from: https://github.com/shinh/elvm/blob/3eddd7e16f67fed6092bebffe3476d2e523a1588/target/util.c#L61)
static const char* general_hs_value_str(Value* v) {
  if (v->type == REG) {
    // NOTE: Emit line (Side effect)
    emit_line("%s <- lift (readRef %sRef)", reg_names[v->reg], reg_names[v->reg]);
    return reg_names[v->reg];
  } else if (v->type == IMM) {
    return format("%d", v->imm);
  } else {
    error("invalid value");
  }
}

// (NOTE: Has side effect)
static const char* general_hs_src_str(Inst* inst) {
  return general_hs_value_str(&inst->src);
}

// (NOTE: Has side effect)
// (from: https://github.com/shinh/elvm/blob/3eddd7e16f67fed6092bebffe3476d2e523a1588/target/util.c#L87)
static const char* general_hs_cmp_str(Inst* inst, const char* true_str) {
  int op = normalize_cond(inst->op, 0);
  const char* op_str;
  switch (op) {
    case JEQ:
      op_str = "=="; break;
    case JNE:
      op_str = "/="; break;
    case JLT:
      op_str = "<"; break;
    case JGT:
      op_str = ">"; break;
    case JLE:
      op_str = "<="; break;
    case JGE:
      op_str = ">="; break;
    case JMP:
      return true_str;
    default:
      error("oops");
  }
  emit_line("%s <- lift (readRef %sRef)", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
  return format("%s %s %s", reg_names[inst->dst.reg], op_str, general_hs_src_str(inst));
}


static void general_hs_n_inc_indent(int n_times) {
  for(int i = 0; i < n_times; i++)
    inc_indent();
}

static void general_hs_n_dec_indent(int n_times) {
  for(int i = 0; i < n_times; i++)
    dec_indent();
}

static void general_hs_emit_func_prologue(int func_id) {
  emit_line("");
  general_hs_n_inc_indent(2);
  emit_line("let func%d :: t m ()", func_id);
  emit_line("    func%d = do", func_id);
  general_hs_n_inc_indent(5);
  
  emit_line("let whileLoop :: t m ()");
  emit_line("    whileLoop = do");
  general_hs_n_inc_indent(5);
  emit_line("pc <- lift (readRef pcRef)");
  emit_line("if %d <= pc && pc < %d",
            func_id * CHUNKED_FUNC_SIZE, (func_id + 1) * CHUNKED_FUNC_SIZE);
  inc_indent();
  emit_line("then do");
  inc_indent();
  emit_line("case pc of");
  inc_indent();
  emit_line("-1 -> return () -- dummy");
  inc_indent();
}

static void general_hs_emit_func_epilogue(void) {
  dec_indent();
  dec_indent();
  emit_line("lift (modifyRef pcRef (+1))");
  emit_line("exits <- lift (readRef exitsRef)");
  emit_line("if exits then return () else whileLoop");
  dec_indent();
  emit_line("else return ()");
  dec_indent();
  general_hs_n_dec_indent(5);
  emit_line("whileLoop");
  general_hs_n_dec_indent(5);
  general_hs_n_dec_indent(2);
}

static void general_hs_emit_pc_change(int pc) {
  emit_line("");
  dec_indent();
  emit_line("%d -> do", pc);
  inc_indent();
}

static void general_hs_emit_inst(Inst* inst) {
  switch (inst->op) {
  case MOV:
    emit_line("lift (writeRef %sRef %s)", reg_names[inst->dst.reg], general_hs_src_str(inst));
    break;

  case ADD:
    emit_line("%s <- lift (readRef %sRef)", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    emit_line("lift (writeRef %sRef ((%s + %s) .&. " UINT_MAX_STR "))",
              reg_names[inst->dst.reg],
              reg_names[inst->dst.reg], general_hs_src_str(inst));
    break;

  case SUB:
    emit_line("%s <- lift (readRef %sRef)", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    emit_line("lift (writeRef %sRef ((%s - %s) .&. " UINT_MAX_STR "))",
              reg_names[inst->dst.reg],
              reg_names[inst->dst.reg], general_hs_src_str(inst));
    break;

  case LOAD:
    emit_line("%s <- lift (readArray mem %s)", reg_names[inst->dst.reg], general_hs_src_str(inst));
    emit_line("lift (writeRef %sRef %s)", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    break;

  case STORE:
    emit_line("%s <- lift (readRef %sRef)", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    emit_line("lift (writeArray mem %s %s)", general_hs_src_str(inst), reg_names[inst->dst.reg]);
    break;

  case PUTC:
    emit_line("putInt %s", general_hs_src_str(inst));
    break;

  case GETC:
    emit_line("%s <- getInt",
              reg_names[inst->dst.reg]);
    emit_line("lift (writeRef %sRef %s)",
              reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    break;

  case EXIT:
    emit_line("lift (writeRef exitsRef True)");
    break;

  case DUMP:
    break;

  case EQ:
  case NE:
  case LT:
  case GT:
  case LE:
  case GE:
    emit_line("lift (writeRef %sRef (if %s then 1 else 0))",
              reg_names[inst->dst.reg], general_hs_cmp_str(inst, "True"));
    break;

  case JEQ:
  case JNE:
  case JLT:
  case JGT:
  case JLE:
  case JGE:
  case JMP:
    emit_line("if (%s) then (lift (writeRef pcRef (%s - 1))) else return ()",
              general_hs_cmp_str(inst, "True"), general_hs_value_str(&inst->jmp));
    break;

  default:
    error("oops");
  }
}

static int general_hs_init_state(Data* data) {
  int prev_mc = -1;
  for (int mp = 0; data; data = data->next, mp++) {
    if (data->v) {
      int mc = mp / 1000;
      while (prev_mc != mc) {
        if (prev_mc != -1) {
          emit_line("return ()");
          general_hs_n_dec_indent(5);
        }
        prev_mc++;
        emit_line("let init%d :: t m ()", prev_mc);
        emit_line("    init%d = do", prev_mc);
        general_hs_n_inc_indent(5);
      }
      emit_line("lift (writeArray mem %d %d)", mp, data->v);
    }
  }
  emit_line("return ()");

  if (prev_mc != -1) {
    general_hs_n_dec_indent(7);
  }

  return prev_mc + 1;
}

void target_general_hs(Module* module) {
  emit_line("{-# LANGUAGE FlexibleContexts #-}");
  emit_line("{-# LANGUAGE AllowAmbiguousTypes #-}");
  emit_line("{-# LANGUAGE MultiParamTypeClasses #-}");
  emit_line("{-# LANGUAGE ScopedTypeVariables #-}");
  emit_line("{-# LANGUAGE FlexibleInstances #-}");
  emit_line("{-# LANGUAGE TypeApplications #-}");
  emit_line("{-# LANGUAGE NamedFieldPuns #-}");
  emit_line("{-# LANGUAGE Strict #-}");
  emit_line("");
  emit_line("import Data.IORef");
  emit_line("import Data.Char");
  emit_line("import Data.Bits");
  emit_line("import Control.Monad.Catch");
  emit_line("");
  emit_line("import qualified Data.Array.MArray as MArray");
  emit_line("import Data.Array.MArray (MArray)");
  emit_line("import qualified Data.Array.IO as Array.IO");
  emit_line("import qualified Data.Array.ST as Array.ST");
  emit_line("import Data.Ix");
  emit_line("import Data.IORef");
  emit_line("import Data.STRef");
  emit_line("import Control.Monad.Trans");
  emit_line("import Control.Monad.Trans.Identity");
  emit_line("import Control.Monad.Trans.State");
  emit_line("import Control.Monad.ST");
  emit_line("");
  emit_line("");
  emit_line("class Monad m => GetPutInt m where");
  emit_line("  getInt :: m Int");
  emit_line("  putInt :: Int -> m ()");
  emit_line("");
  emit_line("class Monad m => MRef m r where");
  emit_line("  newRef    :: a -> m (r a)");
  emit_line("  readRef   :: r a -> m a");
  emit_line("  writeRef  :: r a -> a -> m ()");
  emit_line("  modifyRef :: r a -> (a -> a) -> m ()");
  emit_line("");
  emit_line("class (Ix i, MArray a e m)  => MArrayWithIx a i e m where");
  emit_line("  newArray :: (i, i) -> e -> m (a i e)");
  emit_line("  newArray = MArray.newArray");
  emit_line("");
  emit_line("  newArray_ :: (i, i) -> m (a i e)");
  emit_line("  newArray_ = MArray.newArray_");
  emit_line("");
  emit_line("  readArray :: a i e -> i -> m e");
  emit_line("  readArray = MArray.readArray");
  emit_line("");
  emit_line("  writeArray :: a i e -> i -> e -> m ()");
  emit_line("  writeArray = MArray.writeArray");
  emit_line("");
  emit_line("data InOut = InOut {input :: [Int], output :: [Int]} deriving (Show, Eq)");
  emit_line("");
  emit_line("defaultInOut :: InOut");
  emit_line("defaultInOut = InOut{input=[], output=[]}");
  emit_line("");
  emit_line("instance GetPutInt IO where");
  emit_line("  getInt =");
  emit_line("    catch (do");
  emit_line("        a <- fmap ord getChar");
  emit_line("        return a");
  emit_line("     )");
  emit_line("     (\\(SomeException e) -> return 0)");
  emit_line("  putInt = putChar . chr");
  emit_line("");
  emit_line("instance GetPutInt m => GetPutInt (IdentityT m) where");
  emit_line("  getInt = lift getInt");
  emit_line("  putInt = lift . putInt");
  emit_line("");
  emit_line("instance Monad m => GetPutInt (StateT InOut m) where");
  emit_line("  getInt = do");
  emit_line("    xs <- gets input");
  emit_line("    case xs of");
  emit_line("      [] -> return 0");
  emit_line("      x:xs -> do");
  emit_line("       modify (\\inOut -> inOut{input=xs})");
  emit_line("       return x");
  emit_line("  putInt i = do");
  emit_line("    modify (\\inOut@InOut{output} -> inOut{output=output ++ [i]})");
  emit_line("");
  emit_line("instance MRef IO IORef where");
  emit_line("  newRef = newIORef");
  emit_line("  readRef = readIORef");
  emit_line("  writeRef = writeIORef");
  emit_line("  modifyRef = modifyIORef");
  emit_line("");
  emit_line("instance MRef (ST s) (STRef s) where");
  emit_line("  newRef = newSTRef");
  emit_line("  readRef = readSTRef");
  emit_line("  writeRef = writeSTRef");
  emit_line("  modifyRef = modifySTRef");
  emit_line("");
  emit_line("instance MArrayWithIx Array.IO.IOUArray Int Int IO");
  emit_line("instance MArrayWithIx (Array.ST.STArray s) Int Int (ST s)");
  emit_line("");  
  emit_line("main :: IO ()");
  emit_line("main = runIdentityT (generalMain @Array.IO.IOUArray @IdentityT @IO @IORef)");
  emit_line("");    
  emit_line("generalMain :: forall a t m r. (MonadTrans t, MArrayWithIx a Int Int m, GetPutInt (t m), MRef m r) => t m ()");
  emit_line("generalMain = do");
  inc_indent();
  emit_line("exitsRef <- lift (newRef False) :: t m (r Bool)");
  for (int i = 0; i < 7; i++) {
    emit_line("%sRef <- lift (newRef 0) :: t m (r Int)", reg_names[i]);
  }
  emit_line("mem <- lift (newArray (0, %d) 0) :: t m (a Int Int)", (1<<24)-1);

  int num_inits = general_hs_init_state(module->data);

  CHUNKED_FUNC_SIZE = 128;
  int num_funcs = emit_chunked_main_loop(module->text,
                                         general_hs_emit_func_prologue,
                                         general_hs_emit_func_epilogue,
                                         general_hs_emit_pc_change,
                                         general_hs_emit_inst);

  inc_indent();
  inc_indent();  
  
  for (int i = 0; i < num_inits; i++) {
    emit_line("init%d", i);
  }

  emit_line("");
  emit_line("let mainLoop :: t m ()");
  emit_line("    mainLoop = do");
  general_hs_n_inc_indent(5);
  emit_line("pc <- lift (readRef pcRef)");
  emit_line("case pc `div` %d .|. 0 of", CHUNKED_FUNC_SIZE);
  inc_indent();
  for (int i = 0; i < num_funcs; i++) {
    emit_line("%d -> func%d", i, i);
  }
  dec_indent();
  emit_line("exits <- lift (readRef exitsRef)");
  emit_line("if exits then return () else mainLoop");
  general_hs_n_dec_indent(5);
  emit_line("mainLoop");

}
