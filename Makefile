##
## EPITECH PROJECT, 2022
## B-FUN-400-MAR-4-1-compressor-maori.dino
## File description:
## Makefile
##

NAME    =    imageCompressor

all	:
# 	ghci app/Main.hs
	stack build
	cp	$(shell stack path --local-install-root)/bin/ImageCompressor-exe $(NAME)

clean	:
	stack	clean

fclean	:	clean
	rm	-f	$(NAME)

re	:	fclean	all

.PHONY	:	all	clean	fclean	re