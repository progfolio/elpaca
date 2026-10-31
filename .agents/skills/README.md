# Elpaca agent skills

Skills that let an AI coding agent help Elpaca users debug problems and get
basic support. Each folder holds a `SKILL.md` (frontmatter with `name` and
`description`, then instructions) in the Agent Skills format.

| Skill | Use it for |
|---|---|
| [`elpaca-support`](elpaca-support/SKILL.md) | Usage questions, concepts, update workflow, preparing a good issue |
| [`elpaca-debug`](elpaca-debug/SKILL.md) | Failed or blocked orders, warnings, errors, packages not loading |
| [`elpaca-reproduce`](elpaca-reproduce/SKILL.md) | Building a clean `elpaca-test` reproduction |

## Using them

The canonical location is `.agents/skills/`, which is read by many agent
harnesses (for example Codex, Gemini CLI, GitHub Copilot, Cursor and OpenCode).
Claude Code reads `.claude/skills/` instead, so this repository includes a
`.claude/skills` symlink to `../.agents/skills`. If your checkout does not
support symlinks (e.g. some Windows setups), copy the folders into
`.claude/skills/` in this repository, or into `~/.claude/skills/` to use them
in every project.

To use them against your own Emacs configuration rather than this repository,
copy the folders into that project's skills directory for your agent.

The skills are read-only guidance: they tell the agent to ask before editing
your config, deleting packages, or filing issues.

## Maintaining them

The skills quote current command names, error messages and defaults (for
example the installer version and the warning texts in `elpaca.el`). When you
change one of those, update the matching skill in the same pull request. Where
the wiki and the source disagree, the skills follow the source.
