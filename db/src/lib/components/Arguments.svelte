<script>
  export let args;
</script>

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
