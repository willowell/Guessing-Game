import main from '../src/lib.ts'

if (import.meta.main) {
  await main(Deno.args);
}
