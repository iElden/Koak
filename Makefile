NAME = koak

BIN_NAME = koak-exe

all:	$(NAME)

$(NAME):
	stack build
	cp `stack path | grep local-install-root | cut -f 2 -d ':'`/bin/$(BIN_NAME) ./$(NAME)

clean:
	@echo "Tout est maintenant tout beau tout propre !"

fclean:
	$(RM) $(NAME)

re:	fclean all
