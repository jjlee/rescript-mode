Emacs major mode for ReScript, part of [experimental](#support) Emacs support
for the [ReScript Language](https://rescript-lang.org/).

New ReScript users are advised to use one of the [editors with official support
from the ReScript
team](https://rescript-lang.org/docs/manual/latest/editor-plugins).

## How to Get it Working

Apart from a working [ReScript install](https://rescript-lang.org/docs/manual/latest/installation) and this code, for a full setup you need:

* Strongly recommended: Emacs 27.0 or newer built with native JSON support, for LSP performance
* [rescript-vscode](https://github.com/rescript-lang/rescript-vscode) to provide the ReScript LSP server for type information, compiler errors, completion, and jump to definition/find references
* Currently, [a way to format ReScript code](#formatting) (LSP also provides this, but there is an lsp-mode bug I need to report/fix before this works)

The instructions here assume that you're using [LSP mode](https://emacs-lsp.github.io/lsp-mode/).  [eglot](https://github.com/joaotavora/eglot) may work too, I haven't tried it.

### rescript-vscode

TODO: bundle this or provide a way of auto-installing it

* To build from source, fetch [the rescript
repo](https://github.com/rescript-lang/rescript-vscode) and compile it following
[the build documentation
there](https://github.com/rescript-lang/rescript-vscode/blob/master/CONTRIBUTING.md#install-dependencies).
The language server should then be present as `server/out/server.js` (and in
newer rescript-vscode versions, also the analysis native binary
`analysis/rescript-editor-analysis.exe`).

* Alternatively, you can download and unzip a release `.vsix` from
[here](https://github.com/rescript-lang/rescript-vscode/releases) to use a
prebuilt server.  Again note that though it's just the `server.js` whose path
you should use in `'lsp-rescript-server-command`, in recent releases you do need
to keep the whole `server` directory intact so that you also have
`rescript-editor-analysis.exe` at a path relative to `server.js` in its correct
production build location.

### Vanilla Emacs

`rescript-mode` itself does not depend on `lsp-mode`.  `lsp-rescript` provides
configuration code for `lsp-mode` and depends on `rescript-mode`.

Install the following packages (e.g. using `M-x package-install` -- you can use
things like use-package if you like of course):

* lsp-rescript
* lsp-ui

Add the following to your Emacs configuration code (for example to `~/.emacs`):

```elisp
;; Tell `rescript-mode` how to run your copy of `server.js` from rescript-vscode
;; (you'll have to adjust the path here to match your local system):
(customize-set-variable
  'lsp-rescript-server-command
    '("node" "/path/to/rescript-vscode/server/out/server.js" "--stdio"))
(with-eval-after-load 'rescript-mode
  ;; Tell `lsp-mode` about the `rescript-vscode` LSP server
  (require 'lsp-rescript)
  ;; Enable `lsp-mode` in rescript-mode buffers
  (add-hook 'rescript-mode-hook 'lsp-deferred)
  ;; Enable display of type information in rescript-mode buffers
  (require 'lsp-ui)
  (add-hook 'rescript-mode-hook 'lsp-ui-doc-mode))
```

Restart Emacs and open a ReScript `.res` file and you should have all the
features working.

Note that vanilla Emacs handles the LSP prompt for `"Start a build for this
project to get the freshest data?"` on opening a ReScript file in a rather
unfriendly way: you have to hit `TAB` to see the single possible `Start a Build`
response, then hit return.


### Spacemacs

TODO: make a configuration layer

Add `lsp` to the `dotspacemacs-configuration-layers` section of your spacemacs
configuration file (`SPC f e d` to find that file) -- it should look something
like this:

```elisp
dotspacemacs-configuration-layers
'(
  lsp
  )
```

Add `rescript-mode` and `lsp-rescript` to the `dotspacemacs-additional-packages`
section of your spacemacs configuration file -- it should look something like
this:

```elisp
dotspacemacs-additional-packages
'(
  lsp-rescript
  rescript-mode
  )
```

Add this to the `dotspacemacs/user-config` section of your spacemacs
configuration file:

```elisp
;; Tell `rescript-mode` how to run your copy of `server.js` from rescript-vscode
;; (you'll have to adjust the path here to match your local system):
(custom-set-variables
  '(lsp-rescript-server-command
    '("node" "/path/to/rescript-vscode/server/out/server.js" "--stdio")))
(with-eval-after-load 'rescript-mode
  ;; Tell `lsp-mode` about the `rescript-vscode` LSP server
  (require 'lsp-rescript)
  ;; All I remember is something weird happened if this wasn't there :-)
  (spacemacs|define-jump-handlers rescript-mode)
  ;; Enable `lsp-mode` in rescript-mode buffers
  (add-hook 'rescript-mode-hook 'lsp-deferred)
  ;; Enable display of type information in rescript-mode buffers
  (require 'lsp-ui)
  (add-hook 'rescript-mode-hook 'lsp-ui-doc-mode))
```

Restart spacemacs (`SPC q r`) and open a ReScript `.res` file and you should
have all the features working.


### Formatting

You can use a package like
[`format-all`](https://github.com/lassik/emacs-format-all-the-code) or
[`reformatter`](https://github.com/purcell/reformatter.el) to get your code
formatted correctly (i.e. as `bsc -format`, soon to be renamed `rescript
format`, formats it -- this is like `gofmt` for ReScript).  See [this
thread](https://forum.rescript-lang.org/t/rescript-emacs-support-with-rescript-vscode/1056/14)
(I've not tried either of these).

`lsp-mode` will make this part unnecessary when I get around to submitting a fix
for an `lsp-mode` bug that currently causes `lsp-format-buffer` not to work with
rescript-vscode.

#### Formatting vs. Indentation

In case the distinction is unclear:

Formatting: This means that you run an Emacs command, and your whole buffer (or
some section of it that you specify maybe) is magically formatted correctly --
that is, in the way that `bsc -format` formats it.

Indentation: This means that hitting the tab key or the return key (depending
how you have things configured I guess) gives you an approximation of the
“official” formatting of a tool like `bsc -format`. It’s never identical to
proper formatting, but stops you having to pay attention to formatting when
writing code.


## Features

Aside from the usual font-lock and indentation provided by any language major
mode, this is what is provided by LSP:

### Builds and Errors

You should see any errors show up via flycheck -- for me they look like this:

![Flycheck error](./error.png)

These errors only show up when you save.

If you don't see that, `bsb` may not be running on your project.

To provide these UI for these errors, LSP mode falls back to `flymake` if
`flycheck` is not installed, so it's recommended to install the latter.

When you open a `.res` file in your project, you should see a prompt in Emacs in
the minibuffer `"Start a build for this project to get the freshest data?"`.
You can either hit return on `Start Build` to say yes to that and the LSP server
will start a build for you, or `C-g` out of that and run `bsb` yourself however
you usually do that in your rescript project (in my project I run `npm start`).

You may find the UI here (how the `Start Build` option is presented) is a bit
different from how I describe it depending if you're using vanilla emacs or some
configuration that uses a package like `ivy` or `helm` that overrides the
behaviour of `completing-read`.

If you never want to see this prompt you can put this in your configuration:

```elisp
(custom-set-variables '(lsp-rescript-prompt-for-build nil))
```

If you don't see the `"Start a build for this project to get the freshest
data?"` prompt, that may be because a build is already running somehow, or you
may have a stale `.bsb.lock` lock file in your project.

### Type Information

The configuration above enables `lsp-ui-doc-mode`.  Hovering with the mouse or
moving point to some code should give a popup like this:

![Type information](./typeinfo.png)

Here are some other ways to see type information if you don't like it popping up
automatically:

* You can leave `lsp-ui-doc-mode` off and just use `lsp-ui-doc-glance` every
  time you want to see it.
* You can use `lsp-describe-thing-at-point` to see the type in a window instead
  of in a popup overlay.

### Completion

`lsp-mode`'s completion UI is provided by `company-mode`, so take a look at the
docs for the latter for more about that.

### Jump to Definition / Find References

You can use functions like `lsp-find-definition` and `lsp-find-references`.

I believe functions like `xref-find-definitions` and `xref-find-references` also
end up using LSP and seem equivalent to the LSP functions for ReScript purposes.
`lsp-ui-peek-find-definitions` also seems equivalent to `lsp-find-definition`.

`lsp-ui-peek-find-references` is a fancier more GUI-fied version of
`lsp-find-references` which seems to only sometimes work for me (when it does,
you get a complicated overlay -- it looks like a collection of windows but is
not -- in which you can preview references and navigate through them; other
times it seems to jump me, sometimes inaccurately, to the definition).
`lsp-find-references` itself seems to not find references in other files for me,
I haven't yet tested to see what the behaviour is in VS code.

In spacemacs `, g g` (`spacemacs/jump-to-definition`) ends up rather indirectly
using `lsp-ui-peek-find-definitions` (which is also bound directly to `, G d`).
`, g r` is `xref-find-references`.  `, G r` is `lsp-ui-peek-find-references`
(when it shows the overlay, `j` and `k` or `n` and `p` navigate the list of
references, I think `h` and `l` would navigate the file list if I ever saw one,
and `q` quits).

## Problems

If you run into problems with display of compilation errors
(`flycheck`/`flymake` errors), try this to get rid of any stale ReScript build:

* Kill any `bsb` processes
* Remove any .bsb.lock file in your project
* `M-x revert-buffer` on the .res file you're trying to edit

If you run into problems with other things, you can try killing the LSP server,
which will look something like this:

```/path/to/rescript-vscode/bin/node /path/to/rescript-vscode/server/out/server.js --stdio```

and then if LSP doesn't automatically prompt you to restart the server, `M-x lsp`.


## Known Issues

I've barely used this yet, so probably a lot of things are very broken!

See the github issues, but notably:

* Indentation (note below re formatting vs indentation) is a terrible hack: it's
  lifted straight from `js-mode` (`js.el`) with little effort to adapt it to
  ReScript, and without any JSX support.  Nevertheless, aside from JSX it seems
  to work OK.  This doesn't look like a small task to fix, though quite possibly
  just starting with `js.el` and adding the small amount of code in
  rescript-mode.el would result in something useable -- but then the likelihood
  of my fixing any bugs at all would probably drop to zero!  It's also possible
  that a smaller subset of the JSX support can be extracted -- it's not obvious
  to me that that's easy though.  So I'm inclined to lean heavily on `bsc
  -format` code formatting and not worry about JSX indentation until the day our
  Emacs ReScript hero comes.
* Font lock and indentation are broken for things like `let \"try" = true`.
* Formatting with `lsp-format-buffer` is broken because it does not correctly
  handle the response from rescript-vscode because it uses a range like
  `"end":{"line":1.7976931348623157e+308,"character":1.7976931348623157e+308}`
  -- this should be easy to fix and [you can use other means](#formatting) to do
  this.

Packaging issues:

* Teach lsp-mode how to install rescript-vscode, or bundle it
* Add spacemacs layer

## Support

Please do not report issues related to editor support with Emacs upstream to the
ReScript or rescript-vscode projects (neither on the forum nor the github
issues).  For now please use github issues or github discussions on this project
as a place to discuss Emacs ReScript support.

Emacs is NOT SUPPORTED by the ReScript core team, nor rescript-vscode. The core
ReScript team’s focus for editor support is currently on supporting VS Code and
Sublime Text well. So, if you want something that you can be confident is going
to work smoothly and will not go away, use one of the editors listed as
supported by the core ReScript team (currently VS Code and Sublime Text). In
particular, the Emacs support here depends on the LSP server from
rescript-vscode and its `--stdio` switch, neither of which are officially
supported and could be removed in a later version.

So if you have problems with Emacs and ReScript, please report your issues here,
not upstream with ReScript or rescript-vscode and please don’t complain if you
used ReScript with Emacs and had a bad time – if you did that, you’re going it
alone and you really didn’t try the official ReScript experience – that’s unfair
and a good way to annoy everybody.
