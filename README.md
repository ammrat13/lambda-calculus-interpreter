# lambda-calculus-interpreter
A program that will interpret programs written in the lambda calculus. It accepts files in either standard notation (`λx.x`) with extension `.lca` or in DeBruijn Index notation (`λ 0`) with extension `.ldb`. Output will always be in DeBruijn Index notation.

# To Compile and Run:
  1. `git clone https://github.com/ammrat13/lambda-calculus-interpreter.git`
  2. `cd lambda-calculus-interpreter/src`
  3. `ghc -O2 -odir ../out/o -hidir ../out/hi -o ../lcainter.exe Main.hs`
  4. `cd ..`
  5. `lcainter path/to/input/file`

# File Structure:
An input file has many lambda expressions, each on their own line. Blank lines are ignored. Each expression has the form `let name = body`, where `body` is a lambda expression. Lambda expressions must use `\` in place of `λ`. The expression must be in standard notation if the file ends with `.lca` or in DeBruijn Index notation if the file ends in `.ldb`. Each expression can substitute expressions defined before it into it by their name. After the file is parsed, the expression corresponding to the name `main` is fully reduced by beta and eta reduction, and the reduced expression is printed on screen in DeBruijn Index form.

# TODO:
In order of significance:
  1. Add example files
  2. Allow for `import` statements
  3. Use the `λ` character instead of `\`
  4. Use the `Text` library instead of `String`s
  5. Output in Standard Notation
