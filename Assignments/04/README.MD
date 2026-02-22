# Assignment 4 Tests

Put the project directory `Assignment4` (the innermost one) into the
root directory of this solution, `Assignments`:

```
FsEx/
  |--- Assignments.sln
  |--- 01/
  |--- 02/
  |--- 03/
  |--- 04/
         |--- 04.fsproj
         |--- Tests.fs
         |--- README.md <-- THIS FILE
  |--- Assignment4
         |--- Assignment4.fsproj
         |--- Eval.fs
         |--- Language.fs
```

Run the tests by executing `dotnet run` while standing in this directory.

Executing `dotnet test` should also work, but will require you to be
able to compile all the other test projects, too, meaning your `Assignment3.fs`
file will need to be in the `03` directory.