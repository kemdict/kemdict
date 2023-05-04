import { List, Appbar } from "react-native-paper";
import * as FS from "expo-file-system";
import * as SQLite from "expo-sqlite";
import { Asset } from "expo-asset";
import useSWR from "swr";
import { FlatList, Text, View } from "react-native";

/**
 * Read the database.
 */
async function readDB(): Promise<SQLite.WebSQLDatabase> {
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
      Asset.fromModule(require("./assets/entries.db")).uri,
      FS.documentDirectory + "SQLite/entries.db"
    );
    console.log("done");
  }
  return SQLite.openDatabase("entries.db");
}

export default function Page() {
  const { data, isLoading } = useSWR("dummy", async () => {
    const db = await readDB();
    return await new Promise((resolve) => {
      db.transaction((tx) => {
        tx.executeSql(
          `select * from "heteronyms" limit 10;`,
          [],
          (_, resultSet) => {
            resolve(resultSet.rows._array);
          },
          (_, _err) => {
            resolve([{ title: "error" }]);
          }
        );
      });
    });
  });
  return (
    <>
      {isLoading ? (
        <View>
          <Text>Loading</Text>
        </View>
      ) : (
        <FlatList
          ListHeaderComponent={
            <Appbar.Header>
              <Appbar.Content title="Kemdict" />
            </Appbar.Header>
          }
          data={data}
          renderItem={({ item }) => (
            <View>
              <List.Item
                title={item.title}
                description={JSON.stringify(item)}
              />
            </View>
          )}
          keyExtractor={(item) => item.title}
        ></FlatList>
      )}
    </>
  );
}
