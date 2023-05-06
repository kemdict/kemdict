import { Provider as PaperProvider } from "react-native-paper";
import { Appbar } from "react-native-paper";
import { FlatList, Text, View } from "react-native";
import { StatusBar } from "expo-status-bar";
import { NavigationContainer } from "@react-navigation/native";
import { createNativeStackNavigator } from "@react-navigation/native-stack";
import { StyleSheet, SafeAreaView } from "react-native";

const Stack = createNativeStackNavigator();

import HomeScreen from "./pages/Home";
import SearchScreen from "./pages/Search";

export default function App() {
  return (
    <PaperProvider>
      <NavigationContainer>
        <Stack.Navigator
          initialRouteName="Home"
          screenOptions={{ headerShown: false }}
        >
          <Stack.Screen name="Home" component={HomeScreen} />
          <Stack.Screen name="Search" component={SearchScreen} />
        </Stack.Navigator>
      </NavigationContainer>
      <StatusBar style="auto" />
    </PaperProvider>
  );
}

const styles = StyleSheet.create({
  container: {
    // flex: 1,
    // backgroundColor: "#0B0F14",
    // alignItems: "center",
    // justifyContent: "center",
  },
  text: {
    // color: "#fff",
  },
});
