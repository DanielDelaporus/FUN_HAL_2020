##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile to build project
##

CC		= 	stack
EXEC	= 	hal

all: $(EXEC)

$(EXEC):
	$(CC) build --work-dir ./.stack-work  --allow-different-user
	$(CC) install --local-bin-path .
	mv HAL-exe ./$(EXEC)

clean:
	$(CC) clean
	rm -f HAL.cabal

tests_run:
	$(CC) test --coverage

fclean: clean
	rm -f $(EXEC)
	$(CC) purge

re:		fclean all

.PHONY: 	all clean fclean re