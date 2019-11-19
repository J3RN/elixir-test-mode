# elixir-test

This package gives you a set of incredible handy functions to run Elixir tests:

- `elixir-test-at-point`
- `elixir-test-file`
- `elixir-test-directory`
- `elixir-test-all`
- `elixir-test-rerun-last`

## Getting started

While running these commands is fun, it's more fun when you set a prefix for the keymap:

```
(define-key elixir-test-mode-map (kbd <your keybind here>) 'elixir-test-command-map)
```

For instance, my prefix is `C-c e`, and setting that prefix looks like this:

```
(define-key elixir-test-mode-map (kbd "C-c e") 'elixir-test-command-map)
```

Once you've done that, you have the following keybindings at your disposal:

| Keybind      | Command                                                             |
|--------------|---------------------------------------------------------------------|
| `<prefix> s` | `elixir-test-at-point`                                              |
| `<prefix> f` | `elixir-test-file`                                                  |
| `<prefix> d` | `elixir-test-directory`                                             |
| `<prefix> a` | `elixir-test-all`                                                   |
| `<prefix> l` | `elixir-test-rerun-last`                                            |
| `<prefix> u` | `elixir-test-up` (run tests at the next highest level; WIP)         |
| `<prefix> .` | `elixir-test-failed` (rerun only tests taht failed in the last run) |

## TODO

- [ ] Have a feature you want to see? Open an issue! :smile:
