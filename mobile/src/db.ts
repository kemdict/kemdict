import * as FS from "expo-file-system";
import * as SQLite from "expo-sqlite";
import { Asset } from "expo-asset";

import { format } from "common";
import { CrossDB } from "common/db";

/**
 * Read the database.
 */
export async function readDB(): Promise<SQLite.WebSQLDatabase> {
  let info: FS.FileInfo;
  // TODO: verify integrity
  // info = await FS.getInfoAsync(FS.documentDirectory + "SQLite/entries.db");
  // if (info.exists) {
  //   await FS.deleteAsync(FS.documentDirectory + "SQLite/entries.db");
  // }
  info = await FS.getInfoAsync(FS.documentDirectory + "SQLite");
  if (!info.exists) {
    await FS.makeDirectoryAsync(FS.documentDirectory + "SQLite");
  }
  info = await FS.getInfoAsync(FS.documentDirectory + "SQLite/entries.db");
  if (!info.exists) {
    console.log("downloading");
    await FS.downloadAsync(
      // require only works with static values. (On Hermes only, I guess??)
      Asset.fromModule(require("../assets/entries.db")).uri,
      FS.documentDirectory + "SQLite/entries.db"
    );
    console.log("done");
  }
  return SQLite.openDatabase("entries.db");
}

export const DB = new CrossDB("rn", readDB);
