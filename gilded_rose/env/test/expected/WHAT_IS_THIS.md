# What is `env/test`?

This directory is to facilitate running your COBOL tests in a controlled
directory and then asserting on the contents of that directory afterwards.

You might use this to test a program in which you read from and then wrote to 
some files.

To see this in action, change the contents of 
`env/test/expected/WHAT_IS_THIS.md` and then run `./cbl test` to see the
failure.
