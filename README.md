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
- [ ] A bigger standard library
- [ ] Unit tests
- [ ] Support loading files
- [X] Separate code and data types for Expr using phantom types
- [ ] More robust interactive console
- [ ] `define-macro` and `gensym`
- [X] Implement pretty-printing
- [x] Implement a parser
