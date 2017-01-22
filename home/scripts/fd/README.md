fd
==

`fd()` bash shortcut for cd'ing deep into a directory

fd is short for find directory.  It's a shortcut for finding a directory by name and cd'ing into it.  The use case is for deeply nested directories, where you know the name of final directory but don't want to type all the stuff in the middle.

* fd supports tab completion.  
* For ambiguous paths, fd will prompt the user with a list of all possible paths.
* Tab completion and the select prompt both work with white space.

![demo-gif](https://raw.github.com/sagotsky/fd/master/fd-demo.gif)

Example
==

I work on a drupal distribution.  Drupal uses deeply nested paths.  If I'm in `/var/www/drupal` and I want to get to `/var/www/drupal/sites/all/modules/contrib/og/og_access`, all I do is type `fd og_access`.  Find will take care of the rest of the path.

If I'm feeling super lazy or have forgotten the name of the path, I'd type `fd og_` and hit tab twice, to get a list of the paths beging with og_.

Known limitations
==

Find can be slow if you use it in a stupid place.  fd `tab` `tab` in my home directory will appear to freeze bash because it's finding everything.  

White space was trouble.  I won't be surprised if it breaks in older versions of bash.

Installation
==

Just source `fd.sh` in your shell's rc file.

Tab completion depends on your shell

zsh
---

symlink _fd into `~/.zsh/completion/` for tab completion.

bash
----

source `fd.bash_completion.sh` in your bashrc.

