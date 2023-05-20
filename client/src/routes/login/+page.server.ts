import { redirect } from '@sveltejs/kit';
import type { PageServerLoad } from './$types';

export const load = (async ({ cookies, url }) => {
  const params = url.searchParams;

  if (params.get('signout')) {
    cookies.delete('sessionid', {
      path: '/',
    });
    throw redirect(307, '/login');
  }

  return {};
}) satisfies PageServerLoad;