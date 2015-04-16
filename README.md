A subset of Scheme implemented in F#.

You need Visual Studio, MonoDevelop or Xamarin Studio with F# and NuGet support to build this project.

Or you can provision the Vagrant machine, and run the program inside the VM:

```
$ vagrant up
$ vagrant ssh

Inside Vagrant box:
$ cd /vagrant
$ mono ./Main/bin/Debug/Main.exe
```

## TODO
Make sure everything in the SICP are runnable, except mutable `cons` need extra
changes.

- [X] Nested quasiquotes and unquote-splicing
- [ ] `define-macro`, `gensym`: Add evaluation rules while running
- [X] `error`
- [ ] `cond`
- [ ] `display`, `write`, `print`
- [X] `mcons`: mutable cons cells
- [X] `let`, `local`
- [X] `unset!`
- [ ] `load` for loading files
- [ ] Scope manipulation: The scope/env attached to a lambda
    - [ ] Create scope
    - [ ] View and modify scope variables
    - [ ] Manipulate scope pointers
    - [ ] Manipulate scopes of lambdas
    - [ ] Apply function on scope

