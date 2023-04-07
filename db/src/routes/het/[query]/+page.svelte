<script>
  import Arguments from "$lib/components/Arguments.svelte";
  import hljs from "highlight.js";
  import "highlight.js/styles/atom-one-dark.css";
  import { signatures } from "$lib/common";
  export let data;

  $: params = data.params;
  $: searchParams = data.searchParams;

  const args = signatures["/het"].args;
</script>

<div class="my-8 max-h-96 overflow-auto rounded">
  <pre class="text-sm"><code class="hljs"
      >{@html hljs.highlight("json", JSON.stringify(data.data, null, 2))
        .value}</code
    ></pre>
</div>

<a class="link" href="/">Back to top</a>

<h1 class="mt-2 flex text-4xl">
  /het/
  <div>
    {#each Object.entries(params) as [k, v]}
      <div>{v}</div>
      <div class="text-sm">{k}</div>
    {/each}
  </div>
  <div>
    ?{#each Object.entries(searchParams) as [k, v]}
      {k}={v}
    {/each}
  </div>
</h1>

<Arguments {args} />
