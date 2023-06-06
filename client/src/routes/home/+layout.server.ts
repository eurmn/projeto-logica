import { redirect } from '@sveltejs/kit';
import type { LayoutServerLoad } from './$types';
import type { UserData } from '$lib/types';
import { base } from '$app/paths';

export const load = (async ({ cookies }) => {
  const sessionid = cookies.get('sessionid');

  if (!sessionid) throw redirect(307, '/login');

  const res = await fetch('http://localhost:6162/session?sessionId=' + sessionid, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  });

  if (res.status === 404) throw redirect(307, '/login');

  const userId = await res.json();

  const userDataRes = await fetch('http://localhost:6162/user?id=' + userId, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  });

  if (res.ok) {
    const userData = await userDataRes.json() as UserData;
    return {
      userData
    };
  } else {
    throw redirect(307, '/login');
  }
}) satisfies LayoutServerLoad;