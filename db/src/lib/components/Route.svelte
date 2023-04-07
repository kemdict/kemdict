<script>
  import hljs from "highlight.js";
  import "highlight.js/styles/atom-one-dark.css";
  import { signatures } from "$lib/common";
  export let data;
  export let basePath = "/titles";

  $: desc = signatures[basePath].desc;
  $: args = signatures[basePath].args;
  $: params = Object.entries(data.params);
  $: searchParams = Object.entries(data.searchParams);
</script>

<svelte:head>
  <title>Kemdict API - {basePath}</title>
</svelte:head>

<div class="my-8 max-h-96 overflow-auto rounded">
  <pre class="text-sm"><code class="hljs"
      >{@html hljs.highlight("json", JSON.stringify(data.data, null, 2))
        .value}</code
    ></pre>
</div>

<a class="link" href="/">Back to top</a>

<h1 class="mt-2 flex text-4xl">
  {basePath}
  {#if params.length > 0}
    <span>/</span>
    <div>
      {#each params as [k, v]}
        <div>{v}</div>
        <div class="text-sm">{k}</div>
      {/each}
    </div>
  {/if}
  {#if searchParams.length > 0}
    <div class="flex">
      ?{#each searchParams as [k, v]}
        {k}={v}
      {/each}
    </div>
  {/if}
</h1>

{#if desc}
  <p>{desc}</p>
{/if}

<dl class="mt-2">
  {#each args as { name, desc, children }}
    <dt class="font-bold">{name}</dt>
    <dd class="pl-4">
      <p>{desc}</p>
      {#if children}
        <ul class="pl-4">
          {#each children as child}
            <li class="before:content-['-_']">{child}</li>
          {/each}
        </ul>
      {/if}
    </dd>
  {/each}
</dl>
