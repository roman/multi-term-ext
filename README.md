# multi-term-ext

multi-term is a great tool for managing multiple terminal buffers on
your emacs editor, however the package misses some _like to have_
features like persistent sessions or remote terminals.

This package provides:

* persistent terminal sessions using GNU screen.
* terminal buffer on a remote hosts using ssh.
* persistent terminal sessions on remote hosts using ssh and GNU screen.

## Supported platforms

This package has been tested with GNU emacs 24.1.1 and GNU screen 4.00.03

## How to install

Please refer to installation using [el-get](https://github.com/dimitri/el-get)

## Dependencies

This package requires [multi-term.el](https://github.com/emacsmirrors/multi-term).

## Usage

This package provides 3 main interactive functions:

* `multi-term-persistent` which asks for a GNU screen session name.

* `multi-term-remote` which asks for a SSH address in the form of
  user@host.com

* `multi-term-remote-persistent` which asks for both a GNU screen
  session name and a SSH address.

### Customizations

* In case you want to connect to a remote host through SSH using a port different
  than 22. Use the `multi-term-remote-ssh-port' option.

  ```elisp
  (setq multi-term-remote-ssh-port "2222")

  (multi-term-remote-persistent "vagrant@127.0.0.1" "irb")

  ```

* You can also run other interactive process like irb or ipython using
  a multi-term-remote-persistent like so

  ```elisp
  ;; assuming your irb binary is on /usr/local/bin on the remote machine
  (multi-term-remote-persistent "vagrant@127.0.0.1" "irb" "/usr/local/bin/irb")
  ```

## License

This package is under the GNU General Public License.

Copyright (C) 2012, 2013 Roman Gonzalez.
