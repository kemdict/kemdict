<script lang="ts">
  interface Props {
    id?: string | undefined;
    children?: import('svelte').Snippet;
  }

  let { id = undefined, children }: Props = $props();
  let Pn: HTMLButtonElement;
</script>

<button title="複製" bind:this={Pn} onclick={() => navigator.clipboard.writeText(Pn.innerText)} class="hover:opacity-75 active:opacity-90 fade cursor-pointer my-2 text-gray-800 dark:text-gray-200 text-lg" {id}>{@render children?.()}</button>
