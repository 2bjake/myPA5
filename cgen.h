#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   std::vector<CgenNodeP> ordered_nodes;
   ostream& str;
   int objectclasstag;
   int ioclasstag;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int customclasstag_start;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_dispatch_table(Symbol clazz, std::vector<std::pair<Symbol, Symbol> > methods);
   void code_dispatch_tables();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void process_features(CgenNodeP node, std::vector<attr_class*> attrs, std::vector<std::pair<Symbol, Symbol> > dispatch_tbl, std::map<Symbol, int> method_pos);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private:
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
   std::vector<std::pair<Symbol, Symbol> > dispatch_table;
   std::vector<attr_class*> attrs;
   std::map<Symbol, int> attr_pos;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   void set_dispatch_table(std::vector<std::pair<Symbol, Symbol> > dispatch_table) { this->dispatch_table = dispatch_table; }
   std::vector<std::pair<Symbol, Symbol> > get_dispatch_table() { return dispatch_table; }

   void set_attrs(std::vector<attr_class*> attrs) {
      this->attrs = attrs;
      for (int i = 0; i < attrs.size(); i++) {
         attr_pos[attrs[i]->name] = i;
      }
   }
   std::vector<attr_class*> get_attrs() { return attrs; }

   int get_attr_pos(Symbol name) { return attr_pos[name]; }
};

class BoolConst
{
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

