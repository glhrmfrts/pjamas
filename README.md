# pjamas

> **P**ascal to **Ja**vascript Package **Ma**nagement **S**oftware.

This is a simple non-official package manager focused on Pas2JS, the Pascal-to-JavaScript transpiler. It just downloads the dependencies you want and turn them into a series of `-Fu` flags for the Pas2JS compiler.

**This is a work in progress.**

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
pjamas get github.com/glhrmfrts/wetween --create
```

And to compile:

```bash
pas2js -Jc -Jirtl.js `pjamas units` myprogram.pas
```

Use `-h` or `--help` for more commands.

### pjamas.json

In the above example, this file will be created in the current directory as **pjamas.json**:

```json
{
  "Compiler": "Pas2JS",
  "PackagesDir": "../pjamas-packages",
  "UnitsDirs": [
  ],
  "Dependencies": {
    "github.com/glhrmfrts/wetween": "master"
  }
}
```

You can add other dependencies using `pjamas get` or by adding in the `Dependencies` JSON field.

Use `pjamas help-json` for an extended documentation of each field in the JSON.

**This file should be commited to version control.**

### pjamas-packages

When you download packages, pjamas will download and install them to a directory named `pjamas-packages` in your
package directory. Dependencies that depends on other packages will also install them in this directory, and so on and so forth.

**This directory should be ignored by version control.**

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
- [ ] Cache the output to a file (to use pjamas as a one-time-use tool)
- [ ] Handle other type of dependencies than github repositories
- [ ] Handle private repositories
- [ ] Have an index of known packages (Name + URL + Description)

## LICENSE

MIT