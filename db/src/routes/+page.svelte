<script>
  import { signatures } from "$lib/common";
</script>

<div class="mt-16">
  <h1 class="mb-4 text-5xl">Kemdict API</h1>
  <p>
    Kemdict queries this API to get the data. You can also just download the raw
    data from the sources, build the database from that, then query it yourself.
  </p>
  <ul>
    {#each Object.entries(signatures) as [k, { signature }]}
      <li>
        <a class="link" href={k}>{signature}</a>
      </li>
    {/each}
  </ul>
</div>
