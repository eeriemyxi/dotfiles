# Gruvbox colorscheme for ranger
from ranger.gui.colorscheme import ColorScheme
from ranger.gui.color import default_colors, reverse, bold, normal, default

class Gruvbox(ColorScheme):
    progress_bar_color = 142  # Gruvbox yellow

    def use(self, context):
        fg, bg, attr = default_colors

        if context.reset:
            return default_colors

        elif context.in_browser:
            if context.selected:
                attr = reverse
            else:
                attr = normal
            if context.empty or context.error:
                fg = 167  # Gruvbox red
                bg = 235  # Gruvbox dark gray
            if context.border:
                fg = 244  # Gruvbox gray
            if context.image:
                fg = 109  # Gruvbox aqua
            if context.video:
                fg = 174  # Gruvbox purple
            if context.audio:
                fg = 108  # Gruvbox green
            if context.document:
                fg = 223  # Gruvbox light gray
            if context.container:
                attr |= bold
                fg = 167  # Gruvbox red
            if context.directory:
                attr |= bold
                fg = 142  # Gruvbox yellow
            elif context.executable and not \
                    any((context.media, context.container,
                         context.fifo, context.socket)):
                attr |= bold
                fg = 142  # Gruvbox yellow
            if context.socket:
                fg = 175  # Gruvbox purple
                attr |= bold
            if context.fifo or context.device:
                fg = 208  # Gruvbox orange
                if context.device:
                    attr |= bold
            if context.link:
                fg = 142 if context.good else 108
                bg = 234  # Gruvbox dark background
            if context.bad:
                bg = 235  # Gruvbox dark gray
            if context.tag_marker and not context.selected:
                attr |= bold
                fg = 167  # Gruvbox red
            if not context.selected and (context.cut or context.copied):
                fg = 142  # Gruvbox yellow
                bg = 234
            if context.main_column:
                if context.selected:
                    attr |= bold
                if context.marked:
                    attr |= bold
                    fg = 223  # Gruvbox light gray
            if context.badinfo:
                if attr & reverse:
                    bg = 167  # Gruvbox red
                else:
                    fg = 167

        elif context.in_titlebar:
            attr |= bold
            if context.hostname:
                fg = 167 if context.bad else 142  # Gruvbox red/yellow
            elif context.directory:
                fg = 142  # Gruvbox yellow
            elif context.tab:
                if context.good:
                    bg = 142  # Gruvbox yellow
            elif context.link:
                fg = 108  # Gruvbox green

        elif context.in_statusbar:
            if context.permissions:
                if context.good:
                    fg = 108  # Gruvbox green
                elif context.bad:
                    fg = 167  # Gruvbox red
            if context.marked:
                attr |= bold | reverse
                fg = 223  # Gruvbox light gray
            if context.message:
                if context.bad:
                    attr |= bold
                    fg = 167  # Gruvbox red
            if context.loaded:
                bg = self.progress_bar_color
            if context.vcsinfo:
                fg = 108  # Gruvbox green
                attr &= ~bold
            if context.vcscommit:
                fg = 208  # Gruvbox orange
                attr &= ~bold

        if context.text:
            if context.highlight:
                attr |= reverse

        if context.in_taskview:
            if context.title:
                fg = 108  # Gruvbox green

            if context.selected:
                attr |= reverse

            if context.loaded:
                if context.selected:
                    fg = self.progress_bar_color
                else:
                    bg = self.progress_bar_color

        if context.vcsfile and not context.selected:
            attr &= ~bold
            if context.vcsconflict:
                fg = 167  # Gruvbox red
            elif context.vcschanged:
                fg = 208  # Gruvbox orange
            elif context.vcsunknown:
                fg = 208  # Gruvbox orange
            elif context.vcsstaged:
                fg = 108  # Gruvbox green
            elif context.vcssync:
                fg = 108  # Gruvbox green
            elif context.vcsignored:
                fg = 244  # Gruvbox gray

        elif context.vcsremote and not context.selected:
            attr &= ~bold
            if context.vcssync:
                fg = 108  # Gruvbox green
            elif context.vcsbehind:
                fg = 167  # Gruvbox red
            elif context.vcsahead:
                fg = 108  # Gruvbox green
            elif context.vcsdiverged:
                fg = 167  # Gruvbox red
            elif context.vcsunknown:
                fg = 208  # Gruvbox orange

        return fg, bg, attr
