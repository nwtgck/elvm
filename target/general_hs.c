#include <ir/ir.h>
#include <target/util.h>

// (NOTE: Has side effect)
// (from: https://github.com/shinh/elvm/blob/3eddd7e16f67fed6092bebffe3476d2e523a1588/target/util.c#L61)
static const char* general_hs_value_str(Value* v) {
  if (v->type == REG) {
    // NOTE: Emit line (Side effect)
    emit_line("%s <- readIORef %sRef", reg_names[v->reg], reg_names[v->reg]);
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
  emit_line("%s <- readIORef %sRef", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
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
  emit_line("let func%d :: IO ()", func_id);
  emit_line("    func%d = do", func_id);
  general_hs_n_inc_indent(5);
  
  emit_line("let whileLoop :: IO ()");  
  emit_line("    whileLoop = do");
  general_hs_n_inc_indent(5);
  emit_line("pc <- readIORef pcRef");
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
  emit_line("modifyIORef pcRef (+1)");
  emit_line("exits <- readIORef exitsRef");
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
    emit_line("writeIORef %sRef %s;", reg_names[inst->dst.reg], general_hs_src_str(inst));
    break;

  case ADD:
    emit_line("%s <- readIORef %sRef", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    emit_line("writeIORef %sRef ((%s + %s) .&. " UINT_MAX_STR ")",
              reg_names[inst->dst.reg],
              reg_names[inst->dst.reg], general_hs_src_str(inst));
    break;

  case SUB:
    emit_line("%s <- readIORef %sRef", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    emit_line("writeIORef %sRef ((%s - %s) .&. " UINT_MAX_STR ")",
              reg_names[inst->dst.reg],
              reg_names[inst->dst.reg], general_hs_src_str(inst));
    break;

  case LOAD:
    emit_line("%s <- readArray mem %s", reg_names[inst->dst.reg], general_hs_src_str(inst));
    emit_line("writeIORef %sRef %s", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    break;

  case STORE:
    emit_line("%s <- readIORef %sRef", reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    emit_line("writeArray mem %s %s", general_hs_src_str(inst), reg_names[inst->dst.reg]);
    break;

  case PUTC:
    emit_line("putChar(chr %s)", general_hs_src_str(inst));
    break;

  case GETC:
    emit_line("catch (do");
    general_hs_n_inc_indent(5);
    emit_line("%s <- fmap ord getChar",
              reg_names[inst->dst.reg]);
    emit_line("writeIORef %sRef %s",
              reg_names[inst->dst.reg], reg_names[inst->dst.reg]);
    general_hs_n_dec_indent(5);
    general_hs_n_inc_indent(2);
    emit_line(")");
    emit_line("(\\(SomeException e) -> writeIORef %sRef 0)", reg_names[inst->dst.reg]);
    general_hs_n_dec_indent(2);
    break;

  case EXIT:
    emit_line("writeIORef exitsRef True");
    break;

  case DUMP:
    break;

  case EQ:
  case NE:
  case LT:
  case GT:
  case LE:
  case GE:
    emit_line("writeIORef %sRef (if %s then 1 else 0)",
              reg_names[inst->dst.reg], general_hs_cmp_str(inst, "True"));
    break;

  case JEQ:
  case JNE:
  case JLT:
  case JGT:
  case JLE:
  case JGE:
  case JMP:
    emit_line("if (%s) then (writeIORef pcRef (%s - 1)) else return ()",
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
        emit_line("let init%d :: IO ()", prev_mc);
        emit_line("    init%d = do", prev_mc);
        general_hs_n_inc_indent(5);
      }
      emit_line("writeArray mem %d %d", mp, data->v);
    }
  }
  emit_line("return ()");

  if (prev_mc != -1) {
    general_hs_n_dec_indent(7);
  }

  return prev_mc + 1;
}

void target_general_hs(Module* module) {
  emit_line("import Data.Array.IO");
  emit_line("import Data.IORef");
  emit_line("import Data.Char");
  emit_line("import Data.Bits");
  emit_line("import System.Exit");
  emit_line("import Control.Exception");
  emit_line("");  
  emit_line("main :: IO ()");
  emit_line("main = do");  
  inc_indent();
  emit_line("exitsRef <- newIORef False ::IO (IORef Bool)");
  for (int i = 0; i < 7; i++) {
    emit_line("%sRef <- newIORef 0 :: IO (IORef Int)", reg_names[i]);
  }
  emit_line("mem <- newArray (0, %d) 0 :: IO (IOArray Int Int)", (1<<24)-1);

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
  emit_line("let mainLoop :: IO ()");
  emit_line("    mainLoop = do");
  general_hs_n_inc_indent(5);
  emit_line("pc <- readIORef pcRef");
  emit_line("case pc `div` %d .|. 0 of", CHUNKED_FUNC_SIZE);
  inc_indent();
  for (int i = 0; i < num_funcs; i++) {
    emit_line("%d -> func%d", i, i);
  }
  dec_indent();
  emit_line("exits <- readIORef exitsRef");
  emit_line("if exits then return () else mainLoop");
  general_hs_n_dec_indent(5);
  emit_line("mainLoop");

}
