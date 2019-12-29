# Mort's Keymap

My key remapping for [Karabiner-Elements][karabiner], which replaced
[Karabiner][] as of OS X "High Sierra".

It attempts to make the built-in OS X MacBook (UK)and the external Microsoft
Digital Media (UK) keyboards that I use behave identically, across at least
iTerm2 and Emacs.

It mostly succeeds.

For reference:
  * Complex modifications:
    <https://github.com/pqrs-org/KE-complex_modifications>
  * Keycodes:
    <https://github.com/tekezo/Karabiner-Elements/blob/master/src/apps/PreferencesWindow/PreferencesWindow/Resources/simple_modifications.json>

Install from <https://github.com/mor1/rc-karabiner>, and [read this blog
post](http://mort.io/blog/2017/10/30/spring-loading-karabiner/) for an
explanation of what's going on.

[Karabiner]: https://pqrs.org/osx/karabiner/

Per pages above, to reload Karabiner:

```bash
launchctl kickstart \
  -k gui/$(id -u)/org.pqrs.karabiner.karabiner_console_user_server
```
