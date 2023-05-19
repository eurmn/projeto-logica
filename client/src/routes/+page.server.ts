import { error, redirect } from '@sveltejs/kit';
import type { PageServerLoad } from './$types';

export const load = (async ({ cookies }) => {
  const sessionid = cookies.get('sessionid');

  const res = await fetch('http://localhost:6162/user', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      username: 'John Doe',
      id: 2,
      avatarId: 0,
      password: 'password123'
    })
  });

  console.log(await res.json());
  
  const login = await fetch('http://localhost:6162/login', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      username: 'John Doe',
      password: 'password123'
    })
  });

  console.log(await login.json());

  if (!sessionid) {
    throw redirect(307, '/login');
  }

  throw error(404, 'Not found');
}) satisfies PageServerLoad;