import { error, redirect } from '@sveltejs/kit';
import type { RequestHandler } from './$types';
import uid from 'uid-safe';

export const POST = (async ({ request, cookies }) => {
  const { username, password, avatarId } = await request.json();

  let status = 0;
  let id: number;
  let res: Response;

  do {
    id = Math.floor(Math.random() * 1000000);
  
    res = await fetch('http://localhost:6162/user', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ id, username, password, avatarId })
    });

    status = res.status;
  } while (status === 409);

  if (res.status !== 201) throw error(500, 'Internal Server Error');

  console.log(`User ${username} created.`);

  let sessionId = cookies.get('sessionid');

  if (!sessionId) {
    sessionId = await uid(128);
  }
  
  cookies.set('sessionid', sessionId, {
    path: '/',
  });

  const sessionRes = await fetch('http://localhost:6162/session', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({ sessionId, id })
  });

  if (sessionRes.ok) {
    console.log(`Session for ${username} created/updated.`);
    return new Response(null, { status: 200 });
  }

  throw error(500, 'Internal Server Error');
}) satisfies RequestHandler;