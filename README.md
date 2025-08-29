# elixir-test

This package gives you a set of incredibly handy functions to run Elixir tests:

- `elixir-test-at-point`
- `elixir-test-file`
- `elixir-test-directory`
- `elixir-test-all`
- `elixir-test-rerun-last`

## Installation

If you're using Emacs 30 or later, you can use the `:vc` option of `use-package`:

``` elisp
(use-package elixir-test
  :vc (:url "https://github.com/J3RN/elixir-test-mode")
  :hook
  (elixir-ts-mode . elixir-test-mode)
  (elixir-mode . elixir-test-mode))
```

If you are using an older version of Emacs, you will need to clone this project to your machine and use the `:load-path` option instead:

``` elisp
(use-package elixir-test
  :load-path "wherever/you/cloned/the/repo")
  :hook
  (elixir-ts-mode . elixir-test-mode)
  (elixir-mode . elixir-test-mode))
```

## Usage

While running the commands manually is fun, it's more fun when you set a prefix for the keymap:

```
(define-key elixir-test-mode-map (kbd <your keybind here>) 'elixir-test-command-map)
```

For instance, my prefix is `C-c e`, and setting that prefix looks like this:

```
(define-key elixir-test-mode-map (kbd "C-c e") 'elixir-test-command-map)
```

`use-package` has a nice syntax for this; you can update the definition above to contain this binding like so:

``` elisp
(use-package elixir-test
  :vc (:url "https://github.com/J3RN/elixir-test-mode")
  :bind (:map elixir-test-mode-map ("C-c e" . elixir-test-command-map))
  :hook
  (elixir-ts-mode . elixir-test-mode)
  (elixir-mode . elixir-test-mode))
```


Once you've done that, you have the following keybindings at your disposal:

| Keybind      | Command                                                             |
|--------------|---------------------------------------------------------------------|
| `<prefix> s` | `elixir-test-at-point`                                              |
| `<prefix> f` | `elixir-test-file`                                                  |
| `<prefix> d` | `elixir-test-directory`                                             |
| `<prefix> a` | `elixir-test-all`                                                   |
| `<prefix> l` | `elixir-test-rerun-last`                                            |
| `<prefix> u` | `elixir-test-up` (run tests at the next highest level)              |
| `<prefix> .` | `elixir-test-failed` (rerun only tests that failed in the last run) |

### Example configuration (using `use-package`)

``` elisp
(use-package elixir-test
  :vc (:url "https://github.com/J3RN/elixir-test-mode")
  :bind (:map elixir-test-mode-map ("C-c e" . elixir-test-command-map))
  :hook
  (elixir-ts-mode . elixir-test-mode)
  (elixir-mode . elixir-test-mode))
```

## TODO

- [ ] Have a feature you want to see? Open an issue! :smile:
