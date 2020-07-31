
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <algorithm>
#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val,
       equality_test,
       _case_abort,
       _case_abort2,
       _dispatch_abort;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");

  equality_test = idtable.add_string("equality_test");

  // error labels
  _case_abort = idtable.add_string("_case_abort");
  _case_abort2 = idtable.add_string("_case_abort2");
  _dispatch_abort = idtable.add_string("_dispatch_abort");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


// label count, for generating unique labels
// increment after using
int label_count = 0;

// mapping of type name to tag range for the type's hierarchy
std::map<Symbol, std::pair<int, int> > type_to_tag_range;

std::pair<int, int> fill_tag_range_map(CgenNodeP node) {
  int low = node->get_tag();
  int high = low;

  for (List<CgenNode> *children = node->get_children(); children != NULL; children = children->tl()) {
    int child_high = fill_tag_range_map(children->hd()).second;
    high = std::max(high, child_high);
  }
  std::pair<int, int> range = std::make_pair(low, high);
  type_to_tag_range[node->get_name()] = range;
  return range;
}

// mapping of pair<class_symbol, method_symbol> to dispatch table offset
std::map<std::pair<Symbol, Symbol>, int> method_to_offset;
void fill_method_offset_map(CgenNodeP node) {
  std::vector<std::pair<Symbol, Symbol> > tbl = node->get_dispatch_table();
  for (size_t i = 0; i < tbl.size(); i++) {
    method_to_offset[std::make_pair(node->name, tbl[i].second)] = i;
  }
}

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_load(char *dest_reg, RegisterOffset *source, ostream& s)
{
  emit_load(dest_reg, source->offset, source->reg, s);
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_store(char *source_reg, RegisterOffset *dest, ostream& s)
{
  emit_store(source_reg, dest->offset, dest->reg, s);
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

// shift left logical
static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

// jump and link register
static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

// return: jumps to address in $ra
static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

// branch on equal zero
static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char *dest, ostream& str)
{
  emit_load(dest, 1, SP, str);
  emit_addiu(SP, SP, 4, str);
}

//
// Fetch the integer value in an Bool object.
// Emits code to fetch the integer value of the Bool object pointed
// to by register source into the register dest
//
static void emit_fetch_bool(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

static void emit_method_entry(int temporaries_count, ostream &s) {
  // stack pointer is at the location of the new frame pointer
  emit_store(FP, CALLER_FP_OFFSET, SP, s); // save caller's frame pointer
  emit_move(FP, SP, s); // update frame pointer

  emit_store(SELF, CALLER_SELF_OFFSET, FP, s); // save caller's self
  emit_store(RA, RA_OFFSET, FP, s); // save return address

  emit_addiu(SP, SP,  -4 * (3 + temporaries_count), s); // move stack pointer to end of frame
  emit_move(SELF, ACC, s); // update self

}

// restores caller's registers, tears down frame, and returns
// Note: this code does not set up the ACC return value
static void emit_method_exit(int arg_count, int temporaries_count, ostream &s) {
  emit_load(RA, RA_OFFSET, FP, s); // restore return address
  emit_load(SELF, CALLER_SELF_OFFSET, FP, s); // restore caller's self
  emit_load(FP, CALLER_FP_OFFSET, FP, s); // restore caller's frame pointer

  emit_addiu(SP, SP, 4 * (3 + temporaries_count + arg_count), s); // pop stack space for frame, temporaries and args
  emit_return(s); // return
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s); s << LABEL
      << WORD << stringclasstag << endl
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // object size
      << WORD; emit_disptable_ref(Str, s); s << endl
      << WORD;  lensym->code_ref(s);  s << endl; // string length
      emit_string_constant(s,str); // ascii string
      s << ALIGN;
}

void code_proto_string(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(0);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  emit_protobj_ref(Str, s); s << LABEL
      << WORD << stringclasstag << endl
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + 1) << endl // object size
      << WORD; emit_disptable_ref(Str, s); s << endl
      << WORD;  lensym->code_ref(s);  s << endl; // string length
      emit_string_constant(s,""); // ascii string
      s << ALIGN;
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL
      << WORD << intclasstag << endl
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
      << WORD; emit_disptable_ref(Int, s); s << endl
      << WORD << str << endl; // integer value
}

void code_proto_int(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  emit_protobj_ref(Int, s); s << LABEL
      << WORD << intclasstag << endl
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
      << WORD; emit_disptable_ref(Int, s); s << endl
      << WORD << "0" << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL
      << WORD << boolclasstag << endl
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
      << WORD; emit_disptable_ref(Bool, s); s << endl
      << WORD << val << endl; // value (0 or 1)
}

void code_default_init(ostream &s, Symbol type) {
  if (type == Str) {
    stringtable.add_string("")->code_ref(s);
  } else if (type == Int) {
    inttable.add_int(0)->code_ref(s);
  } else if (type == Bool) {
    falsebool.code_ref(s);
  } else {
    s << 0;
  }
}

void code_default_attr(ostream &s, Symbol type) {
  s << WORD; code_default_init(s, type); s << endl;
}

void code_proto_class(ostream &s, Symbol name, std::vector<attr_class*> attrs, int classtag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  emit_protobj_ref(name, s); s << LABEL
      << WORD << classtag << endl
      << WORD << (DEFAULT_OBJFIELDS + attrs.size()) << endl // object size
      << WORD; emit_disptable_ref(name, s); s << endl;

  for (size_t i = 0; i < attrs.size(); i++) {
    code_default_attr(s, attrs[i]->type_decl);
  }
}

void code_class_init(ostream &s, CgenNodeP node, int classtag)
{
  // method init label
  emit_init_ref(node->get_name(), s); s << LABEL;
  if (node->basic()) {
    emit_return(s);
    return;
  }

  // calculate necessary temporaries count
  int temporaries_count = 0;
  std::vector<attr_class*> attrs = node->get_declared_attrs();
  for (size_t i = 0; i < attrs.size(); i++) {
    temporaries_count = std::max(temporaries_count, attrs[i]->init->calc_temporaries());
  }

  // activation record setup
  emit_method_entry(temporaries_count, s);
  if (!node->get_parentnd()->basic()) {
    s << JAL << node->parent << CLASSINIT_SUFFIX << endl;
  }

  SymbolTable<Symbol, RegisterOffset> env = node->make_environment();

  // initialize attributes
  for (size_t i = 0; i < attrs.size(); i++) {
    if (attrs[i]->init->get_type() != NULL) {
      attrs[i]->init->code(node, env, FIRST_TEMPORARY_OFFSET, s);
      RegisterOffset* loc = env.lookup(attrs[i]->name);
      emit_store(ACC, loc, s);
    }
  }

  // teardown and return
  emit_move(ACC, SELF, s);
  emit_method_exit(0, temporaries_count, s);
}

void code_class_name_table(ostream &s, std::vector<CgenNodeP> nodes)
{
  s << CLASSNAMETAB << LABEL;
  for (size_t i = 0; i < nodes.size(); i++) {
    Symbol name = nodes[i]->get_name();
    StringEntryP string_sym = stringtable.lookup_string(name->get_string());
    s << WORD; string_sym->code_ref(s); s << endl;
  }
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
      << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
  enterscope();
  if (cgen_debug) cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();
  order_classes(root());
  fill_tag_range_map(root());

  std::vector<std::pair<Symbol, Symbol> > empty_table;
  std::map<Symbol, int> empty_pos;
  std::vector<attr_class*> empty_attrs;
  process_features(root(), empty_attrs, empty_table, empty_pos);
  for (size_t i = 0; i < ordered_nodes.size(); i++) {
    fill_method_offset_map(ordered_nodes[i]);
  }
  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{
  // The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list.  Thus, these classes exist, but are not part of the
  // inheritance hierarchy.
  // No_class serves as the parent of Object and the other special classes.
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  // prim_slot is a class known to the code generator.
  //
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object    aborts the program
  //        type_name() : Str        returns a string representation of class name
  //        copy() : SELF_TYPE       returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  CgenNodeP object_node = new CgenNode(
    class_(Object,
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this);
  install_class(object_node);
  objectclasstag = add_ordered_node(object_node);

  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  CgenNodeP io_node = new CgenNode(
     class_(IO,
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),
    Basic,this);
  install_class(io_node);
  ioclasstag = add_ordered_node(io_node);

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer.
//
CgenNodeP int_node = new CgenNode(
     class_(Int,
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this);
install_class(int_node);
intclasstag = add_ordered_node(int_node);

  //
  // Bool also has only the "val" slot.
  //
  CgenNodeP bool_node = new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this);
  install_class(bool_node);
  boolclasstag = add_ordered_node(bool_node);

  //
  // The class Str has a number of slots and operations:
  //       val                                  ???
  //       str_field                            the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  CgenNodeP string_node = new CgenNode(
      class_(Str,
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat,
				   single_Formals(formal(arg, Str)),
				   Str,
				   no_expr()))),
	    single_Features(method(substr,
				   append_Formals(single_Formals(formal(arg, Int)),
						  single_Formals(formal(arg2, Int))),
				   Str,
				   no_expr()))),
	     filename),
        Basic,this);
  install_class(string_node);
  stringclasstag = add_ordered_node(string_node);

  customclasstag_start = stringclasstag + 1;
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i)) {
    CgenNodeP node = new CgenNode(cs->nth(i),NotBasic,this);
    install_class(node);
  }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
      set_relations(l->hd());
  }
}

void CgenClassTable::order_classes(CgenNodeP nd) {
  if (!nd->basic()) {
    add_ordered_node(nd);
  }

  for (List<CgenNode> *children = nd->get_children(); children != NULL; children = children->tl()) {
    order_classes(children->hd());
  }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenClassTable::code_dispatch_table(Symbol clazz, std::vector<std::pair<Symbol, Symbol> > methods) {
    emit_disptable_ref(clazz, str); str << LABEL;
    for (size_t i = 0; i < methods.size(); i++) {
      str << WORD; emit_method_ref(methods[i].first, methods[i].second, str);
      str << endl;
    }
}

void CgenClassTable::code_dispatch_tables() {
  for (std::vector<CgenNodeP>::iterator it = ordered_nodes.begin(); it != ordered_nodes.end(); ++it) {
    CgenNodeP node = *it;
    code_dispatch_table(node->get_name(), node->get_dispatch_table());
  }
}

void CgenClassTable::code_methods() {
  for (size_t i = 0; i < methods.size(); i++) {
    method_class* method = methods[i].second;
    emit_method_ref(methods[i].first->get_name(), method->name, str); str << LABEL;
    int temporaries_count = method->expr->calc_temporaries();
    emit_method_entry(temporaries_count, str);

    // get class env & add formals to it
    SymbolTable<Symbol, RegisterOffset> env = methods[i].first->make_environment();
    Formals formals = method->formals;
    for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
      Symbol name = formals->nth(j)->get_name();
      RegisterOffset *offset = new RegisterOffset(j + FIRST_ARG_OFFSET, FP);
      env.addid(name, offset);
    }

    method->expr->code(methods[i].first, env, FIRST_TEMPORARY_OFFSET, str);
    emit_method_exit(method->formals->len(), temporaries_count, str);
  }
}

void CgenClassTable::process_features(CgenNodeP node, std::vector<attr_class*> inherited_attrs, std::vector<std::pair<Symbol, Symbol> > dispatch_tbl, std::map<Symbol, int> method_pos) {
  std::vector<attr_class*> declared_attrs;
  Features features = node->features;
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    if (method_class* method = dynamic_cast<method_class*>(features->nth(i))) {
      // store method for declaration/coding
      if (!node->basic()) {
        methods.push_back(std::make_pair(node, method));
      }

      // store method on class for generating dispatch tables
      std::pair<Symbol, Symbol> entry = std::make_pair(node->name, method->name);
      std::map<Symbol, int>::iterator it = method_pos.find(method->name);
      if (it != method_pos.end()) {
        dispatch_tbl[it->second] = entry;
      } else { // not an override, add to table at next position
        method_pos[method->name] = dispatch_tbl.size();
        dispatch_tbl.push_back(entry);
      }
    } else if (attr_class* attr = dynamic_cast<attr_class*>(features->nth(i))) {
      declared_attrs.push_back(attr);
    }
  }
  node->set_dispatch_table(dispatch_tbl);
  node->set_attrs(inherited_attrs, declared_attrs);
  List<CgenNode> *children = node->get_children();
  inherited_attrs.insert(inherited_attrs.end(), declared_attrs.begin(), declared_attrs.end());
  for (children; children != NULL; children = children->tl()) {
    process_features(children->hd(), inherited_attrs, dispatch_tbl, method_pos);
  }
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

int CgenClassTable::add_ordered_node(CgenNodeP node) {
  node->set_tag(ordered_nodes.size());
  ordered_nodes.push_back(node);
  return node->get_tag();
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();


  code_proto_class(str, Object, ordered_nodes[objectclasstag]->get_all_attrs(), objectclasstag);
  code_proto_class(str, IO, ordered_nodes[ioclasstag]->get_all_attrs(), ioclasstag);
  code_proto_string(str, stringclasstag);
  code_proto_int(str, intclasstag);

  for(size_t i = customclasstag_start; i < ordered_nodes.size(); i++) {
    Symbol name = ordered_nodes[i]->get_name();
    code_proto_class(str, name, ordered_nodes[i]->get_all_attrs(), i);
  }

  code_class_name_table(str, ordered_nodes);

  code_dispatch_tables();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  for(size_t i = 0; i < ordered_nodes.size(); i++) {
    code_class_init(str, ordered_nodes[i], i);
  }

  code_methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  expr->code(so, env, temp_offset, s);
  RegisterOffset *loc = env.lookup(name);
  emit_store(ACC, loc, s);
}

void static_dispatch_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  int arg_count = this->actual->len();
  if (arg_count > 0) {
    emit_addiu(SP, SP, -4 * arg_count, s);
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
      actual->nth(i)->code(so, env, temp_offset, s);
      emit_store(ACC, i + 1, SP, s);
    }
  }
  expr->code(so, env, temp_offset, s);
  emit_bne(ACC, ZERO, label_count, s);
  // deal with void
  emit_load_string(ACC, stringtable.add_string(so->get_filename()->get_string()), s);
  emit_load_imm(T1, 1, s);
  emit_jal(_dispatch_abort->get_string(), s);

  emit_label_def(label_count++, s);
  // load dispatch table
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);

  Symbol dispatch_type = this->type_name; // NOTE: this is the only line that is different than dispatch_class::code(), consider refactor
  if (dispatch_type == SELF_TYPE) {
    dispatch_type = so->name;
  }

  int offset = method_to_offset[std::make_pair(dispatch_type, name)];
  std::cout << dispatch_type << " " << name << " " << offset << endl;
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);
}

void dispatch_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  int arg_count = this->actual->len();
  if (arg_count > 0) {
    emit_addiu(SP, SP, -4 * arg_count, s);
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
      actual->nth(i)->code(so, env, temp_offset, s);
      emit_store(ACC, i + 1, SP, s);
    }
  }
  expr->code(so, env, temp_offset, s);
  emit_bne(ACC, ZERO, label_count, s);
  // deal with void
  emit_load_string(ACC, stringtable.add_string(so->get_filename()->get_string()), s);
  emit_load_imm(T1, 1, s);
  emit_jal(_dispatch_abort->get_string(), s);

  emit_label_def(label_count++, s);
  // load dispatch table
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);

  Symbol dispatch_type = expr->get_type();
  if (dispatch_type == SELF_TYPE) {
    dispatch_type = so->name;
  }

  int offset = method_to_offset[std::make_pair(dispatch_type, name)];
  std::cout << dispatch_type << " " << name << " " << offset << endl;
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);
}

void cond_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  int else_label = label_count++;
  int fi_label = label_count++;
  this->pred->code(so, env, temp_offset, s);
  emit_fetch_bool(ACC, ACC, s);
  emit_beqz(ACC, else_label, s);
  then_exp->code(so, env, temp_offset, s);
  emit_branch(fi_label, s);
  emit_label_def(else_label, s);
  else_exp->code(so, env, temp_offset, s);
  emit_label_def(fi_label, s);
}

void loop_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  int loop_begin_label = label_count++;
  int loop_end_label = label_count++;
  emit_label_def(loop_begin_label, s);
  pred->code(so, env, temp_offset, s);
  emit_fetch_bool(ACC, ACC, s);
  emit_beqz(ACC, loop_end_label, s);
  body->code(so, env, temp_offset, s);
  emit_branch(loop_begin_label, s);
  emit_move(ACC, ZERO, s);
  emit_label_def(loop_end_label, s);
}

bool sort_by_tag_asc(Case a, Case b) {
  int a_tag = type_to_tag_range[a->get_type_decl()].first;
  int b_tag = type_to_tag_range[b->get_type_decl()].first;
  return a_tag > b_tag;
}

void typcase_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  int end_label = label_count++;

  // sort cases by tag value (highest to lowest)
  std::vector<Case> ordered_cases;
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    ordered_cases.push_back(cases->nth(i));
  }
  std::sort(ordered_cases.begin(), ordered_cases.end(), sort_by_tag_asc);

  expr->code(so, env, temp_offset, s);
  emit_load(T2, TAG_OFFSET, ACC, s);
  emit_bne(ACC, ZERO, label_count, s);
  // deal with void
  emit_load_string(ACC, stringtable.add_string(so->get_filename()->get_string()), s);
  emit_load_imm(T1, 1, s);
  emit_jal(_case_abort2->get_string(), s);

  for(size_t i = 0; i < ordered_cases.size(); i++) {
    emit_label_def(label_count++, s);
    std::pair<int, int> range = type_to_tag_range[ordered_cases[i]->get_type_decl()];
    emit_blti(T2, range.first, label_count, s);
    emit_bgti(T2, range.second, label_count, s);

    // store expr in temporary
    RegisterOffset varLoc = RegisterOffset(temp_offset, FP);
    emit_store(ACC, &varLoc, s);
    env.enterscope();

    ordered_cases[i]->get_expr()->code(so, env, temp_offset - 1, s);
    env.exitscope();
    emit_branch(end_label, s);
  }

  // handle no match error
  emit_label_def(label_count++, s);
  emit_jal(_case_abort->get_string(), s);

  // end
  emit_label_def(end_label, s);
}

void block_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(so, env, temp_offset, s);
  }
}

void let_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  if (init->get_type() != NULL) {
    init->code(so, env, temp_offset, s);
  } else if (type_decl == Int || type_decl == Bool || type_decl == Str) {
    s << LA << ACC << " "; code_default_init(s, type_decl); s << endl;
  } else {
    emit_move(ACC, ZERO, s);
  }

  RegisterOffset varLoc = RegisterOffset(temp_offset, FP);
  emit_store(ACC, &varLoc, s);
  env.enterscope();
  env.addid(identifier, &varLoc);
  body->code(so, env, temp_offset - 1, s);
  env.exitscope();
}

#define code_arith(op) \
e1->code(so, env, temp_offset, s); \
emit_store(ACC, temp_offset, FP, s); \
e2->code(so, env, temp_offset - 1, s); \
s << JAL; emit_method_ref(Object, ::copy, s); s << endl; \
emit_load(T1, temp_offset, FP, s); \
emit_fetch_int(T1, T1, s); \
emit_fetch_int(T2, ACC, s); \
op(T1, T1, T2, s); \
emit_store_int(T1, ACC, s);

void plus_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  code_arith(emit_add)
}

void sub_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  code_arith(emit_sub)
}

void mul_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  code_arith(emit_mul)
}

void divide_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  code_arith(emit_div)
}

void neg_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  e1->code(so, env, temp_offset, s);
  s << JAL; emit_method_ref(Object, ::copy, s); s << endl;
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_store_int(T1, ACC, s);
}

#define code_compare(op) \
e1->code(so, env, temp_offset, s); \
emit_store(ACC, temp_offset, FP, s); \
e2->code(so, env, temp_offset - 1, s); \
emit_load(T1, temp_offset, FP, s); \
emit_fetch_int(T1, T1, s); \
emit_fetch_int(T2, ACC, s); \
emit_load_bool(ACC, truebool, s); \
op(T1, T2, label_count, s); \
emit_load_bool(ACC, falsebool, s); \
emit_label_def(label_count++, s);

void lt_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  code_compare(emit_blt)
}

void eq_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  e1->code(so, env, temp_offset, s);
  emit_store(ACC, temp_offset, FP, s);
  e2->code(so, env, temp_offset - 1, s);

  emit_load(T1, temp_offset, FP, s);
  emit_move(T2, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beq(T1, T2, label_count, s);
  emit_load_bool(A1, falsebool, s);
  emit_jal(equality_test->get_string(), s);
  emit_label_def(label_count++, s);
}

void leq_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  code_compare(emit_bleq)
}

void comp_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  e1->code(so, env, temp_offset, s);
  emit_fetch_bool(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beqz(T1, label_count, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label_count++, s);
}

void int_const_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  Symbol name;
  if (type_name == SELF_TYPE) {
    name = so->get_name();
  } else {
    name = type_name;
  }
  s << LA << ACC << " "; emit_protobj_ref(name, s); s << endl;
  s << JAL; emit_method_ref(Object, ::copy, s); s << endl;
  s << JAL; emit_init_ref(name, s); s << endl;
}

void isvoid_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  e1->code(so, env, temp_offset, s);
  emit_move(T1, ACC, s);
  emit_load_bool(ACC, truebool, s);
  emit_beqz(T1, label_count, s);
  emit_load_bool(ACC, falsebool, s);
  emit_label_def(label_count++, s);
}

void no_expr_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  //noop
}

void object_class::code(CgenNode* so, SymbolTable<Symbol, RegisterOffset > env, int temp_offset, ostream &s) {
  if (name == self) {
    emit_move(ACC, SELF, s);
  } else {
    RegisterOffset *loc = env.lookup(name);
    emit_load(ACC, loc, s);
  }
}

// temporaries calculations

int assign_class::calc_temporaries() {
  return expr->calc_temporaries();
}

int static_dispatch_class::calc_temporaries() {
  int temps = expr->calc_temporaries();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    temps = std::max(temps, 1 + actual->nth(i)->calc_temporaries());
  }
  return temps;
}

int dispatch_class::calc_temporaries() {
  int temps = expr->calc_temporaries();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    temps = std::max(temps, 1 + actual->nth(i)->calc_temporaries());
  }
  return temps;
}

int cond_class::calc_temporaries() {
  return std::max(pred->calc_temporaries(), std::max(then_exp->calc_temporaries(), else_exp->calc_temporaries()));
}

int loop_class::calc_temporaries() {
  return std::max(pred->calc_temporaries(), body->calc_temporaries());
}

int typcase_class::calc_temporaries() {
  int temps = expr->calc_temporaries();
  for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
    temps = std::max(temps, 1 + cases->nth(i)->get_expr()->calc_temporaries());
  }
  return temps;
}

int block_class::calc_temporaries() {
  int temps = 0;
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    temps = std::max(temps, body->nth(i)->calc_temporaries());
  }
  return temps;
}

int let_class::calc_temporaries() {
  return std::max(init->calc_temporaries(), 1 + body->calc_temporaries());
}

int plus_class::calc_temporaries() {
  return std::max(e1->calc_temporaries(), 1 + e2->calc_temporaries());
}

int sub_class::calc_temporaries() {
  return std::max(e1->calc_temporaries(), 1 + e2->calc_temporaries());
}

int mul_class::calc_temporaries() {
  return std::max(e1->calc_temporaries(), 1 + e2->calc_temporaries());
}

int divide_class::calc_temporaries() {
  return std::max(e1->calc_temporaries(), 1 + e2->calc_temporaries());
}

int neg_class::calc_temporaries() {
  return e1->calc_temporaries();
}

int lt_class::calc_temporaries() {
  return std::max(e1->calc_temporaries(), 1 + e2->calc_temporaries());
}

int eq_class::calc_temporaries() {
  return std::max(e1->calc_temporaries(), 1 + e2->calc_temporaries());
}

int leq_class::calc_temporaries() {
  return std::max(e1->calc_temporaries(), 1 + e2->calc_temporaries());
}

int comp_class::calc_temporaries() {
  return e1->calc_temporaries();
}

int int_const_class::calc_temporaries() {
  return 0;
}

int string_const_class::calc_temporaries() {
  return 0;
}

int bool_const_class::calc_temporaries() {
  return 0;
}

int new__class::calc_temporaries() {
  return 0;
}

int isvoid_class::calc_temporaries() {
  return e1->calc_temporaries();
}

int no_expr_class::calc_temporaries() {
  return 0;
}

int object_class::calc_temporaries() {
  return 0;
}