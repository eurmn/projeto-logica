import type { Post } from '$lib/types';
import { error } from '@sveltejs/kit';
import type { PageServerLoad } from './$types';

export const load = (async () => {
  const res = await fetch('http://localhost:6162/all_posts', {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    },
  });
  
  const posts = await res.json() as Post[];

  for (const post of posts) {
    const userDataRes = await fetch('http://localhost:6162/user?id=' + post.author, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    });

    if (!userDataRes.ok) {
      throw error(500, 'Internal Server Error');
    }

    post.author = await userDataRes.json();
  }

  return {
    posts
  };
}) satisfies PageServerLoad;