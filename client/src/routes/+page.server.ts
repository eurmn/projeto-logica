import { redirect } from '@sveltejs/kit';
import type { PageServerLoad } from './$types';

export const load = (async ({ cookies }) => {
  const sessionid = cookies.get('sessionid');

  if (!sessionid) {
    throw redirect(307, '/login');
  }

  const res = await fetch('http://localhost:6162/session?sessionId=' + sessionid, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  });

  if (res.status === 404) throw redirect(307, '/login');

  throw redirect(307, '/home');
}) satisfies PageServerLoad;