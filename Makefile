NAME = koak

all:	$(NAME)

$(NAME):
	nix-shell --run "cabal build"
	cp dist-newstyle/build/x86_64-linux/ghc-8.6.5/llvm-demo-0.1.0.0/x/llvm-demo/build/llvm-demo/llvm-demo ./$(NAME)

ghci:
	nix-shell --run "cabal repl"

clean:
	@echo "Tout est maintenant tout beau tout propre !"

fclean:
	$(RM) $(NAME)

re:	fclean all
