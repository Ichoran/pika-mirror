# pika-mirror

**Pika Mirror does not exist yet.  This documents what it will be.**

Pika Mirror will back up old files, some of which may themselves be backups, onto new storage unit(s).

## How to build it

Pika Mirror is written in Scala and built with SBT.  You will have to build it yourself.  It has no dependencies.

## How to use it

PikaMirror is a two-step backup program.  This means it's twice as slow as it might be.  Oh well!

First, it needs to understand the data you want to back up.

```bash
scala -jar Pika.jar understand foo/baz
```

Pika will traverse through the entire directory hierarchy rooted at `foo/baz`, warning about any symlinks that go outside that tree, and create a `pika-understood.json` file in `foo/baz` that contains directory information including hashes of all the binary data in the files.  If the directory already has a `pika-understood.json` file, it will instead check to make sure that the file sizes and dates are all the same, and either update `pika-understood.json` if it needs it, or tell you that everything looks okay.

Pika will be listening to keyboard input while it works.  If you want to abort a run and have it save its partial progress, type `stop` at the command-line and it will stop when it is convenient to do so.

Once Pika has understood a directory, it can mirror it.  You can tell it

```bash
scala -jar Pika.jar mirror foo/baz as tybble into bar/quux
```

If `bar/quux` is absent, or is present but completely empty, it'll just copy everything over, and write a `pika-understood.json` file that explains that this is a mirroring destination.  If it exists, is nonempty, and is not already marked as a mirroring destination, then Pika will report an error and exit.  If it exists and has an `pika-understood.json` file, it will merge the directory structures as cleverly as it can, removing duplicates along the way.  `tybble` is a name used to identify this merge; Pika remembers where things came from, even if it doesn't put new copies there.

If you want to find out where the files actually are, you can

```bash
scala -jar Pika.jar list bar/quux/bippy
```

to see everything that is supposed to be there and where it is now, or

```bash
scala -jar Pika.jar moved bar/quux/bippy
```

to show just those things that have been moved.

If you just want it to reconstruct some old source, then

```bash
scala -jar Pika.jar reconstruct bar/quux/bippy into tozz/wreb
```

and it will recreate the old tree as faithfully as it can.

If you want to know what Pika is mirroring, ask it

```bash
scala -jar Pika.jar who bar/quux
```

and it will say who has been put in there.  Any subdirectory is fine also.

Pika will try to understand and reproduce symbolic links within the hierarchy.  It will also try to detect duplicate directories and directory trees and sensibly merge them, moving them if necessary.  In every case it will actually preserve all the data; its job is to not lose anything.  (Of course, any program may have bugs, so you should perform basic sanity checking on the results if they're important.)

Pika does not like anything to be deleted from within a mirror.  If you want something gone, your only option is to make the mirror directory the source for `mirror`, delete the branches that you want, and have it copy the entire thing to a new mirror directory.  Along the way, it will lose track of where everything came from.  You have been warned!

If you want to make a duplicate copy of a mirror, you can

```bash
scala -jar Pika.jar sync bar/quux with yar/quuux
```

and Pika will figure out in which direction copies need to be made and will copy things over.  (It's okay if one of the two is empty and has no `pika-understood.json` file.)  If copies need to be made in both directions, Pika will panic and quit.  It's your job to figure out who is correct, or make the merges manually, and then sync the authoritative version with a new directory.
