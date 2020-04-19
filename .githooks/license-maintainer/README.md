# license-maintainer
Preconfigured for C# and JS/TS. See original repo for details.

# Requirements

* Git for Windows

# Install globally

1. Copy files from `.githooks\license-maintainer` to `C:\Users\YOUR_PROFILE\.githooks`
2. Add following to `C:\Users\YOUR_PROFILE\.gitconfig`
```
[core]
	hooksPath = C:\\Users\\YOUR_PROFILE\\.githooks
	attributesfile = C:\\Users\\YOUR_PROFILE\\.githooks\\.gitattributes
[license]
	author = YOUR_COMPANY
```
3. Configure `C:\Users\YOUR_PROFILE\.githooks\.gitattributes`
3. Configure `C:\Users\YOUR_PROFILE\.githooks\pre-commit`