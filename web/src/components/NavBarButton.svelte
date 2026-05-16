<script lang="ts">
  export let text: string;
  export let href: string;
  export let pathname: string | undefined = undefined;
  $: active = pathname
    ? pathname === href
    : globalThis?.location?.href !== undefined &&
      new URL(location.href).pathname === href;

  console.log({
    active,
    href,
    pathname,
    locationHrefPathname:
      globalThis?.location?.href !== undefined &&
      new URL(location.href).pathname,
  });
</script>

<a {href} class="flex flex-col items-center justify-center gap-1 text-center">
  <div class="h-6 w-6">
    {#if active}
      <slot name="active" />
    {:else}
      <slot name="normal" />
    {/if}
  </div>
  <div class="text-xs">{text}</div>
</a>
