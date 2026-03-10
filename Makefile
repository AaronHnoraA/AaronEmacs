install:
	emacs --debug-init -q -l ./bootstrap.el

clean:
	echo "Cleaning......"
	rm -rf ./var

default:
	install
