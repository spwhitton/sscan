# sscan -- Sean's scanner

## Synopsis

sscan is a UI for scanning multi-page documents using the scanimage(1)
command and a flatbed scanner.

I wrote sscan because I often need to scan multi-page documents on my
GNU/Linux system, and I have only a flatbed scanner, without an ADF.

## Screenshot

[later]

## Installation

1. Ensure that your scanner has [SANE support][].
2. Install prerequisite utilities: `apt-get install ocrmypdf pdftk
   sane-utils haskell-stack`.
3. Obtain sscan source: `git clone https://git.spwhitton.name/sscan`
4. Optionally, modify the file `Presets.hs` so that it contains
   settings presets for the scanning tasks you most often need to
   perform.
5. Use [stack][] to build and install sscan: `cd sscan && stack
   install`.
   
Note that stack will download various dependencies automatically.  I
hope that future versions of sscan will require only dependencies
available from the Debian mirrors.

[SANE support]: http://www.sane-project.org/sane-supported-devices.html
[stack]: https://haskellstack.org/

## Usage

Open a terminal, run `sscan` and follow the on-screen instructions.

## Bugs

Please report bugs by e-mail to `<spwhitton@spwhitton.name>`.

## License

Copyright (C) 2017  Sean Whitton

sscan is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

sscan is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with sscan.  If not, see
[<http://www.gnu.org/licenses/>](http://www.gnu.org/licenses/).

