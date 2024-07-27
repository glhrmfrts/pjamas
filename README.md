# pjamas

> **P**ascal to **Ja**vascript Package **Ma**nagement **S**oftware.

This is a simple non-official package manager focused on Pas2JS, the Pascal-to-JavaScript transpiler. It just downloads the dependencies you want and turn them into a series of `-Fu` flags for the Pas2JS compiler.

## How to use

Let's say we have a simple program that wants to use this code: https://github.com/glhrmfrts/wetween.

```pascal
program myprogram;
uses WeTween;
var
  mgr: TWeManager;
begin
  mgr := TWeManager.Create(nil);
  // go bananas
end.
```

We can just use the `get` command inside our project root:

```bash
pjamas get github.com/glhrmfrts/wetween
```

And to compile:

```bash
pas2js -Jc -Jirtl.js `pjamas units` myprogram.pas
```

Or alternatively, pjamas can invoke pas2js for you with the `build` command (pas2js must be on the PATH):

```bash
pjamas build myprogram.pas
```

Use `-h` or `--help` for more commands.

## How to build

The only requirement is a standard installation of FPC and Lazarus, the versions used in the development are:

- Free Pascal Compiler version 3.2.2
- Lazarus v3.0

Then build using Lazarus or use the command-line:

```bash
fpc pjamas.lpr
```

Which will produce a **pjamas** binary.

## TODO

The scope of this project shall remain small, and help is needed:

- [ ] Documentation
- [ ] Cache the output to a file
- [ ] Handle other type of dependencies than github repositories
- [ ] Handle private repositories
- [ ] A simple way to list known packages (Name + URL + Description)

## LICENSE

MIT