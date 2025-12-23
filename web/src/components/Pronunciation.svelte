<script lang="ts">
  export let id: string | undefined = undefined;
</script>

<div class="my-2 text-gray-800 dark:text-gray-200 text-lg" {id}><slot /></div>
