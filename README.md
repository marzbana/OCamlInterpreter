Canvas ```markdown
# My OCaml Interpreter for a Made-Up Language

This repository contains an interpreter for a fictional stack-based language, written in OCaml. It demonstrates how to parse and evaluate commands, manage a stack of values, and handle control flow structures like `IfThen-Else-End` blocks and user-defined functions. The code includes examples of pushing constants, performing arithmetic operations, manipulating the stack, and working with control flow constructs.

## Table of Contents

1. [About the Project](#about-the-project)  
2. [Tech Stack](#tech-stack)  
3. [Getting Started](#getting-started)  
4. [How It Works](#how-it-works)  
5. [Language Overview](#language-overview)  
6. [Demo](#demo)  
7. [Challenges](#challenges)

---

## About the Project

This project was created to explore how interpreters work under the hood. It takes a script containing commands in a made-up language and processes each line using a stack-based approach. Operations include:

- **Arithmetic** (`Add`, `Sub`, `Mul`, `Div`)  
- **Boolean and Comparison** (`And`, `Or`, `Not`, `Equal`, `Lte`)  
- **Stack manipulation** (`Push`, `Pop`, `Swap`, `Neg`, `Concat`)  
- **Control flow** (`IfThen`, `Else`, `End`, `Begin`)  
- **User-defined functions** (`Fun`, `Call`, `Return`)

While it's a toy language, it illustrates the general principles of parsing input, evaluating commands, and maintaining a run-time environment (via a stack and an environment list).

## Tech Stack

- **OCaml** – A powerful functional programming language that excels in parsing and compiler/interpreter projects.  

## Getting Started

Follow these steps to set up and run the project locally on your machine:

1. **Clone the repository**  
   ```bash
   git clone https://github.com/your-username/your-ocaml-interpreter.git
   cd your-ocaml-interpreter
   ```

2. **Install OCaml**  
   - Make sure you have OCaml installed. You can use [OPAM](https://opam.ocaml.org/) or your system’s package manager.  
   - Verify installation by running:
     ```bash
     ocaml --version
     ```

3. **Compile the Interpreter**  
   - Compile the main file:
     ```bash
     ocamlopt -o interpreter interpreter.ml
     ```

4. **Run the Interpreter**  
   - If using the compiled executable:
     ```bash
     ./interpreter script.txt output.txt
     ```
     Here, `script.txt` is your source code file containing the made-up language commands, and `output.txt` is where the program’s final state/stack is printed.

5. **Modify the Script**  
   - You can modify or create new scripts (`.txt` files) with commands like `Push`, `Add`, `IfThen`, etc., and rerun the interpreter to see how it responds.

## How It Works

- **Parsing Commands**  
  Each line is parsed to identify a command (e.g., `Push`, `Pop`, `Add`) along with any optional arguments (like integers or strings).

- **Stack Operations**  
  The language relies heavily on pushing and popping values from a stack. Commands such as `Add` pop the top two integers, add them, and push the result back.

- **Control Flow**  
  - `IfThen`, `Else`, `End` blocks let you conditionally execute parts of the script.  
  - `Begin`, `End` let you start a local block (scope) and revert to the previous scope on exit.

- **Functions**  
  - Defining functions with `Fun <funcName> <paramName>` and calling them with `Call`.  
  - The environment stores closures (`Clo`) that capture the function’s parameter and body.

**Example**: A simple script might look like this:
```
Push 5
Push 10
Add
IfThen
  Push "Greater"
  Print
Else
  Push "Smaller"
  Print
End
Quit
```

## Language Overview

Here’s a quick cheat sheet of some commands:

- **Push X**: Push integer/string `X` onto the stack.  
- **Pop**: Pop the top element from the stack.  
- **Add/Sub/Mul/Div**: Arithmetic operations on the top two stack elements.  
- **And/Or/Not**: Boolean operations on integers `0` or `1`.  
- **IfThen/Else/End**: Control flow for conditional statements.  
- **Begin/End**: Begin a new scope, end the current scope.  
- **Fun/Call/Return**: Define and invoke functions.  
- **Quit**: Print the stack to the output file and exit.

## Challenges

The most difficult aspect of building this interpreter was handling **nested control flow** and **function scoping**. Ensuring that each `IfThen`, `Begin`, or `Fun` had a matching `End` (and properly capturing the environment for user-defined functions) required careful bookkeeping in both the code for parsing commands and the code that executes those commands. Overcoming this involved implementing helper functions to slice input strings, track current scope depths, and maintain multiple environment frames for nested blocks and function calls. Through thorough testing and step-by-step debugging, these features were integrated successfully.
```

