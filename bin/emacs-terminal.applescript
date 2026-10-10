on run
    set launcherPath to POSIX path of (path to home folder) & ".config/emacs/bin/emacs-terminal"
    do shell script quoted form of launcherPath
end run

on open droppedItems
    set launcherPath to POSIX path of (path to home folder) & ".config/emacs/bin/emacs-terminal"
    repeat with oneFile in droppedItems
        do shell script (quoted form of launcherPath) & " " & quoted form of POSIX path of oneFile
    end repeat
end open
