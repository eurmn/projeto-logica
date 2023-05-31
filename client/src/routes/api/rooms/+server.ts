import { error, json } from '@sveltejs/kit';
import type { RequestHandler } from './$types';

export const POST = (async ({ request }) => {
  const { userId } = await request.json();
  const roomCode = Math.random().toString(36).substring(2, 8).toUpperCase();

  const res = await fetch('http://localhost:6162/room', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      owner: userId,
      roomCode
    })
  });

  if (res.ok) {
    console.log(`Room ${roomCode} created.`);
    return json(roomCode, { status: 200 });
  }

  throw error(500, 'Internal Server Error');
}) satisfies RequestHandler;


export const DELETE = (async ({ request }) => {
  const { roomCode } = await request.json();

  const res = await fetch('http://localhost:6162/room/delete', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      roomCode
    })
  });

  if (res.ok) {
    console.log(`Room ${roomCode} deleted.`);
    return json(roomCode, { status: 200 });
  }

  throw error(500, 'Internal Server Error');
}) satisfies RequestHandler;