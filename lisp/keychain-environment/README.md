Load keychain environment variables
===================================

[Keychain] is a script that manages ssh-agent and gpg-agent.  It is
typically run from the shell's initialization file.  It allows your
shells and cron jobs to share a single `ssh-agent` and/or `gpg-agent`.

When keychain is run, it checks for running agent, otherwise it
starts them.  It saves the agents' environment variables to files
inside `~/.keychain/`, so that subsequent shells can source these
files.

When Emacs is started under X11 and not directly from a terminal
these variables are not set.  This library looks for these files
created by keychain and then sets Emacs' environment variables
accordingly.  It does not actually run keychain, so you still
have to run that from a login shell first.

To use run the function `keychain-refresh-environment` in your
init file.  If keychain has not been run yet when you start Emacs
you can also later call that function interactively.

[Keychain]: http://www.funtoo.org/wiki/Keychain
