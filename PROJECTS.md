# Soluna Projects

Soluna have some features for a smoother experience. Here is how it works.

## Creating a new project and how the structure works

To create a new Soluna project simply use this command `soluna new project-name`. This will create a folder using the name provided.

Here is the structure:

```
project-name/
├── libs/
├── main.luna
└── project.sol
```

The `libs` folder will contains all the libraries imported (we will talk about that in a bit).

The main.luna is the default `entry`. An entry is the file that will use Soluna to start the build process.

Finally the project.sol is used for two things: let Soluna understand that it's a project that it can handle, and it used to configure your project.

It currently contains only 3 things, `name`, `entry`, and `version`. The last one is only for you, nothing related to Soluna (yet, might change in the future).

The project name will be used as output name for the bundled executable and the entry is, as said before, the file that will be used by Soluna to start the bundling process.

## Rules for the `project.sol` file

No spaces, option name first, value second (also no spaces) with a ':' character between them.

It's not recommended to tweak too much this file, it might causes issues as Soluna is still really fresh on these.

If the entry isn't where you want, named as you want, or anything you can obviously change it to something that suits your needs.

## Adding external libraries

You can either add libraries manually or automatically. If you do it manually it's recommended that you follow the "Soluna way".

- Create a folder inside `libs` that is the lib name
- Add source files inside it

And yes, there is an automatic libraries importer. Soluna is using an [official libs repositories](https://github.com/L0Wigh/soluna-libs).

To import something from it just use `soluna add libname` and Soluna will try it's best to import it for you. It uses curl, so check if you have it on your computer. You most likely have it (yes, even on Windows).

## Build made easy

You can now simply use `soluna build` to bundle your project without have to feed it the entry file manually.

## Final words

Keep in mind that it's a very early version of the build system. Be gentle with it and report any bugs.
