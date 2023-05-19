import { redirect } from '@sveltejs/kit';
import type { PageServerLoad } from './$types';

export const load = (async ({ cookies }) => {
  const sessionid = cookies.get('sessionid');

  if (!sessionid) {
    throw redirect(307, '/login');
  }
}) satisfies PageServerLoad;