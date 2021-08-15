/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   main.c                                             :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: challeau <marvin@42.fr>                    +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2019/11/30 01:32:27 by challeau          #+#    #+#             */
/*   Updated: 2020/01/22 14:53:03 by challeau         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "../get_next_line.h"

int main(int ac, char **av)
{
	char *line;
	int i = 1;
	int ret = 1;
	int fd;

	if (ac == 1)
	{
		fd = 1;
		while (ret > 0)
		{
			ret = get_next_line(fd, &line);
			if (ret >= 0)
				printf("[%d][LINE %d]: %s\n", ret, i, line);
			if (ret == 0)
				printf("[0][FINISHED]\n");
			if (ret == -1)
			{
				printf("[-1][ERROR]\n");
				return (0);
			}
			free(line);
			line = NULL;
			i++;
		}
	}
	if (ac == 3 && av[1][0] == '1')
	{
		fd = open(av[2], O_RDONLY);
		while (ret > 0)
		{
			ret = get_next_line(fd, &line);
			if (ret > 0)
				printf("%s\n", line);
			else if (ret == -1)
			{
				printf(">>> ERROR <<<\n");
				return (0);
			}
			free(line);
			line = NULL;
			i++;
		}
	}
	if (ac == 3 && av[1][0] == '2')
	{
		fd = open(av[2], O_RDONLY);
		while (ret > 0)
		{
			ret = get_next_line(fd, &line);
			if (ret > 0)
				printf("[%d][LINE %d]: %s\n", ret, i, line);
			if (ret == 0)
				printf("[0][REACHED EOF] %s\n", line);
			if (ret == -1)
			{
				printf("[-1][ERROR]\n");
				return (0);
			}
			free(line);
			line = NULL;
			i++;
		}
	}
	close(fd);
	return (0);
}
