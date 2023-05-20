import { error } from '@sveltejs/kit';
import type { RequestHandler } from './$types';
import uid from 'uid-safe';

export const POST = (async ({ request, cookies }) => {
  const { username, password } = await request.json();

  const res = await fetch('http://localhost:6162/login', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({ username, password })
  });

  if (!res.ok) throw error(401, 'Unauthorized');
  console.log(`User ${username} logged in`);

  const userId = await res.json();

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
    body: JSON.stringify({ sessionId, id: userId })
  });

  if (sessionRes.ok) {
    console.log(`Session for ${username} created/updated.`);
    return new Response(null, { status: 200 });
  }

  throw error(500, 'Internal Server Error');
}) satisfies RequestHandler;