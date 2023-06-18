# format-region.el

Transform region in different formats in Emacs: camelCase, kebab-case or lisp-case, PascalCase or snake_case.

![format-region](demo.gif)

## Usage

Select region and call:

### camelCase

```
M-x format-to-camel-case-region
```

### kebab-case o lisp-case

```
M-x format-to-kebab-case-region
```

or

```
M-x format-to-lisp-case-region
```

### PascalCase

```
M-x format-to-pascal-case-region
```

### snake_case

```
M-x format-to-snake-case-region
```

## Install

Add in your `init.el`.

```elisp
(use-package format-region
  :straight (:host github :repo "tanrax/format-region.el" :files ("format-region.el"))
  :ensure t)
```
