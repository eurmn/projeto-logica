import { error, json } from '@sveltejs/kit';
import type { RequestHandler } from './$types';
import uid from 'uid-safe';

export const POST = (async ({ request }) => {
  const { 
    title, description, userId
  } = await request.json();

  const date = new Date().toISOString();
  const id = await uid(18);

  // find user ID
  const userDataRes = await fetch('http://localhost:6162/user?id=' + userId, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  });

  if (!userDataRes.ok) {
    throw error(401, 'Unauthorized');
  }

  const res = await fetch('http://localhost:6162/post', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({ id, title, description, date, author: (await userDataRes.json()).id })
  });

  if (res.ok) {
    console.log(`Post "${title}" created.`);
    return json({
      id,
      date
    }, { status: 200 });
  }

  console.log(res.status, await res.json());

  throw error(500, 'Internal Server Error');
}) satisfies RequestHandler;
