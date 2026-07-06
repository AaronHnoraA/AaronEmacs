"""
test_command_fix.py

Check the special cases where the kernel command needs fixing up.

"""

from remote_ikernel.manage import command_fix


def test_irkernel_unescaped(capsys):
    """Check that it gets escaped."""
    kernel_command = ("/usr/lib64/R/bin/R --slave -e IRkernel::main() "
                      "--args {connection_file}")
    fixed_kernel_command = ("/usr/lib64/R/bin/R --slave -e 'IRkernel::main()' "
                            "--args {connection_file}")
    assert command_fix(kernel_command) != kernel_command
    out, _err = capsys.readouterr()
    assert "Escaping" in out
    assert command_fix(kernel_command) == fixed_kernel_command


def test_irkernel_already_escaped():
    """Don't escape if it should already work."""
    # single quotes
    kernel_command = ("/usr/lib64/R/bin/R --slave -e 'IRkernel::main()' "
                      "--args {connection_file}")
    assert command_fix(kernel_command) == kernel_command
    # double quotes
    kernel_command = ('/usr/lib64/R/bin/R --slave -e "IRkernel::main()" '
                      '--args {connection_file}')
    assert command_fix(kernel_command) == kernel_command
    # escaped
    kernel_command = (r'/usr/lib64/R/bin/R --slave -e IRkernel::main\(\) '
                      '--args {connection_file}')
    assert command_fix(kernel_command) == kernel_command


def test_unescaped_brackets(capsys):
    """Warn if there are brackets """
    # unescaped cases
    command_fix('one two three ( four five six')
    out, _err = capsys.readouterr()
    assert "unescaped brackets" in out

    command_fix('one two three ) four five six')
    out, _err = capsys.readouterr()
    assert "unescaped brackets" in out

    command_fix(r'one two three (\) four five six')
    out, _err = capsys.readouterr()
    assert "unescaped brackets" in out

def test_escaped_brackets(capsys):
    """Don't warn if it seems fine."""
    # escaped cases
    command_fix(r'one two three \(\) four five six')
    out, _err = capsys.readouterr()
    assert "unescaped brackets" not in out

    command_fix('one two three \'()\' four five six')
    out, _err = capsys.readouterr()
    assert "unescaped brackets" not in out

    command_fix('one two three "()" four five six')
    out, _err = capsys.readouterr()
    assert "unescaped brackets" not in out


def test_missing_quotations(capsys):
    """Warn if command seems broken."""
    # Broken
    command_fix('one two three \' four five six')
    out, _err = capsys.readouterr()
    assert "missing quotation marks" in out
    command_fix('one two three \" four five six')
    out, _err = capsys.readouterr()
    assert "missing quotation marks" in out
    # Fine
    command_fix('one two three \' four \' five six')
    out, _err = capsys.readouterr()
    assert "missing quotation marks" not in out
    command_fix('one two three " four " five six')
    out, _err = capsys.readouterr()
    assert "missing quotation marks" not in out
