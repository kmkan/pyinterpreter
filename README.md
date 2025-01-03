# Modified Pascal Interpreter

This repository contains an interpreter for a modified version of the Pascal programming language made in Python. The interpreter supports several features, including variables, program structure with `BEGIN` and `END` blocks, procedure blocks (no formal parameters), and arithmetic expressions using a Racket-style functional notation.

---

## Features

### 1. **Core Pascal Constructs**
   - **Variables**: Declare and use variables within your program.
   - **Program Structure**: Programs are encapsulated using the `PROGRAM`, `BEGIN`, and `END` blocks.

### 2. **Procedures**
   - Support for declaring procedures using the `PROCEDURE` keyword.
   - No formal parameters yet; procedures are treated as block statements.

### 3. **Arithmetic in Functional Notation**
   - Arithmetic operations are written in a Lisp/Racket-inspired prefix notation:
     ```pascal
     (+ 1 2)    { Adds 1 and 2 }
     (* 3 4 5)   { Multiplies 3, 4, and 5 }
     (DIV 10 2)    { Performs integer division of 10 by 2 }
     ```

### 4. **Variable Declarations**
   - Variables can be declared with the `VAR` keyword and typed as `INTEGER` or `REAL`.
   - Example:
     ```pascal
     VAR x, y: INTEGER;
         z: REAL;
     ```

### 5. **Comments**
   - Pascal-style comments `{ ... }` are supported and ignored during interpretation.

---

## How It Works

1. **Lexical Analysis (Lexer)**
   - The lexer converts raw Pascal source code into a stream of tokens.

2. **Parsing (Parser)**
   - The parser builds an Abstract Syntax Tree (AST) from the tokenized input.

3. **Interpretation (Interpreter)**
   - The interpreter evaluates the AST and executes the program, storing variable states in global memory.

---

## Example Program

Here is a sample Pascal-like program that works with the interpreter:

```pascal
PROGRAM SampleProgram;
VAR
    x, y: INTEGER;
    z: REAL;

BEGIN
    x := 10;
    y := 20;
    z := (+ x y);
END.
```

---

## Running the Interpreter

1. Save your Pascal code in a file, e.g., `program.pas`.

2. Run the interpreter using the command:
   ```bash
   python interpreter.py program.pas
   ```

3. The interpreter will execute the program and print the results of all operations.

---

## Limitations & Work in Progress

- Procedures do not yet support parameters or local variables.
- No support for control structures like `IF` or `WHILE`.
- Currently, only supports integer and real numbers as data types.
- Error handling is basic and needs improvement for user-friendly debugging.

---

## Contributing

Contributions are welcome! Feel free to open issues or submit pull requests to enhance the interpreter.
