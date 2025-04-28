# Chimera

**Chimera** is an educational programming language project designed to explore and implement the most interesting concepts from modern languages (Rust, Swift, etc.) from scratch.  
The goal is to learn **compiler engineering**, **type systems**, and **optimization techniques** by building a real system, not just toy examples.

---

## ✨ Current Features
- **Concrete Syntax Tree (CST)** using [Rowan] for lossless parsing.
- **Pseudo-Abstract Syntax Tree (AST)**: partially abstracted, lazy structure built atop CST.
- **High-Level Intermediate Representation (HIR)**: abstraction layer for analysis and optimizations.
- **Pratt Parsing** for operator precedence (e.g., parsing expressions like `1 + 2 * 3 - 4` correctly).
- **Tensor & Buffer Literals** with lazy traversal for memory efficiency.
- **Hindley-Milner Type Inference** (work in progress).
- **Early Design for Optimizations**:  
  - Common Subexpression Elimination (CSE)
  - Memoization for pure functions
  - Cache-aware tensor layouts and loop transformations
- **Scope Resolution and Metadata Tracking** (work in progress).

---

## 📚 Technical Highlights
- **Rowan-based CST**: fast, lossless syntax tree for initial parsing.
- **Lazy AST Construction**: avoids building redundant subtrees; prepares for fast HIR generation.
- **Tensor Memory Handling**:  
  - Buffers (stack-allocated) have static known shapes.
  - Tensors (heap-allocated) allow partially known dimensions (`tensor<_, _, 3, 3>`).
- **Integrated Typing During Lowering**: AST-to-HIR lowering prepares the type inference context.
- **Error Handling**: span-tracked errors using [miette], delaying expensive source attachment until needed.

---

## 🛠️ Current Focus
- **Complete HIR Construction**: clean AST-to-HIR transformation.
- **Finish Hindley-Milner Typing**:  
  - Support for lambdas, tensor literals, struct literals.
  - Deferred type unifications for incomplete structures.
- **Prepare for Optimization Passes**:
  - CSE based on function purity detection.
  - Basic memoization heuristics.
- **Initial Scope Resolution and Type Context Embedding**:
  - Lightweight metadata for functions, variables, blocks.

---

## 📋 Quick BNF Preview
Example snippet of Chimera’s syntax rules:

```bnf
Item ::= StructItem | EnumItem | ...

StructItem ::= 'struct' Name '{' FieldList '}'

Expr ::= Factor | Expr '+'|'-' Expr

Factor ::= Atom | Factor '*'|'/' Atom

Atom ::= Number | '(' Expr ')'

## 📋 Binding example for Pratt parsing 
    Pratt parsing
    1 + 2 * 3 - 4! 

    1      +      2      *       3      -      4     ?     
       10    10.1    11     11.1   10     10.1    13


## 📋 A quick example for CST -> AST -> HIR 
→ let p = i + 314

CST: 
Root@0..10
  VariableDef@0..10
    LetKw@0..3 "let"
    InfixBinaryOp@3..10
      VariableRef@3..4
        Identifier@3..4 "p"
      Eq@4..5 "="
      InfixBinaryOp@5..10
        VariableRef@5..6
          Identifier@5..6 "i"
        Plus@6..7 "+"
        Literal@7..10
          Number@7..10 "314"

AST:
VarDef(
    VarRef(
        VariableRef@3..4
            Identifier@3..4 "p"
        ,
    ),
    Some(
        Infix(
            Binary(
                InfixBinaryOp@5..10
                    VariableRef@5..6
                    Identifier@5..6 "i"
                    Plus@6..7 "+"
                    Literal@7..10
                    Number@7..10 "314"
                ,
            ),
        ),
    ),
),


HIR:
VarDef(
    VarDef(
        VarRef(
            VariableRef@3..4
                Identifier@3..4 "p"
            ,
        ),
        Some(
            Infix(
                Binary(
                    InfixBinaryOp@5..10
                        VariableRef@5..6
                        Identifier@5..6 "i"
                        Plus@6..7 "+"
                        Literal@7..10
                        Number@7..10 "314"
                    ,
                ),
            ),
        ),
    ),
),


## 🧩 Longer-Term Ideas
	•	Lifetime analysis
	•	Borrow checker logic (inspired by Rust)
	•	Escape analysis and move semantics
	•	Detecting use-after-move
	•	Automatic layout optimizations for tensors/buffers 
	•	Sparse tensor handling 

## 🚀 Why This Project Matters (to me)

Chimera is not just about writing a language —
it’s about learning real-world compiler techniques:
	•	Managing complexity in parsing and type systems.
	•	Thinking about memory layouts and optimizations.
	•	Building infrastructure that can evolve into a JIT or MLIR lowering.

## 🛤️ Near-Term TODOs
	•	Finalize lazy buffer/tensor AST handling.
	•	HIR preparation for types and scopes.
	•	Full Hindley-Milner type inference pass.
	•	Initial Optimization Pass: CSE for pure expressions.
	•	Tests for type unification and scope resolution.


## ⚡ Acknowledgments

Built with heavy inspiration from Rust, Swift, and other modern programming languages.
Core parsing powered by the fantastic Rowan syntax tree library.
