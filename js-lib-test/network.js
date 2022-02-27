const net = require('net');

const client = new net.Socket();

client.connect(4000, '127.0.0.1', () => {
    console.log('链接服务器成功');
    client.write("hey server!");
    client.end();
})

client.on('data', (data) => {
    console.log("received message: ", data.toString());
})

client.on("end", function () {
    console.log("客户端发送数据结束")
});

