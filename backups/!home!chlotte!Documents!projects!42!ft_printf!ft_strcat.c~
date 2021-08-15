#include "libft.h"

char	*ft_strcat(char *dst, char *src)
{
	int i;

	i = 0;
	if (!dst || !src)
	    return (NULL);
	while (dst[i])
		i++;
	while (*src)
	{
		dst[i] = *src;
		i++;
		src++;
	}
	return (dst);
}
