
.PHONY:
clean:
	find . -name compiled -type d -print0 | xargs -0 rm -rf
	find . -name '*.bak' -print0 | xargs -0 rm -f
