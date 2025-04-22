##
## EPITECH PROJECT, 2025
## mypandoc
## File description:
## Makefile
##


###############################################################################
# Configuration
###############################################################################

NAME			= 	mypandoc

###############################################################################
# Sources
###############################################################################

SOURCES			=	app/Main.hs

###############################################################################
# Makefile Objects
###############################################################################

BINARY_PATH    := $(shell stack path --local-install-root)

STACK_OBJ      = .stack-work


###############################################################################
# Makefile logics
###############################################################################

all: $(NAME)

$(NAME): $(SRC)
	stack build
	cp $(BINARY_PATH)/bin/$(NAME) ./$(NAME)


run: $(NAME)
	stack exec $(NAME)

clean:
	stack clean
	rm -rf $(STACK_OBJ)
	rm -rf mypandoc.cabal
	rm -rf app/Main

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all run clean fclean re

