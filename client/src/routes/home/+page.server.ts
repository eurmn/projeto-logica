import type { Room } from '$lib/types';
import { redirect } from '@sveltejs/kit';
import type { LayoutData, PageServerLoad } from './$types';

export const load = (async ({ parent }) => {
  const data = await parent() as LayoutData;

  const res = await fetch('http://localhost:6162/room?id=' + data.userData.id, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  });

  if (res.status === 404) throw redirect(307, '/login');

  const rooms = await res.json() as Room[];

  // console.log(rooms);
  
  /* for (let i = 0; i < rooms.length; i++) {
    for (let j = 0; j < rooms[i].users.length; j++) {
      const userRes = await fetch('http://localhost:6162/user?id=' + rooms[i].users[j], {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      });

      const userData = await userRes.json();

      rooms[i].users[j] = userData;
    }
  } */

  return {
    rooms
  };
}) satisfies PageServerLoad;