/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   ft_itoa.c                                          :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: challeau <marvin@42.fr>                    +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2019/11/05 17:46:13 by challeau          #+#    #+#             */
/*   Updated: 2019/11/11 17:58:10 by challeau         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "libft.h"

static int	ft_intlen(long n)
{
	int len;

	len = 1;
	if (n < 0)
	{
		n *= -1;
		len++;
	}
	while (n >= 10)
	{
		n /= 10;
		len++;
	}
	return (len);
}

char		*ft_itoa(int n)
{
	int		i;
	int		len;
	long	nb;
	char	*dst;

	i = 0;
	nb = n;
	len = ft_intlen(nb);
	if (!(dst = (char *)malloc((len + 1) * sizeof(char))))
		return (NULL);
	if (nb < 0)
	{
		nb *= -1;
		dst[i++] = '-';
	}
	dst[len--] = '\0';
	while (len >= i)
	{
		dst[len--] = (nb % 10) + 48;
		nb /= 10;
	}
	return (dst);
}
