const { createReadStream } = require('fs');
const { join } = require('path');

const stream = createReadStream(join(__dirname, '../data/texts/hamlet.txt'), 'utf-8');
const wordMap = {};

stream.on('data', (data) => {
    let str = data.toString();
    str = str.trim();
    let words = str
        .split(' ')
        .flatMap(v => v.split('\r'))
        .flatMap(v => v.split('\n'))
        .map(v => v.toLowerCase())
        .filter(Boolean);

    words.forEach(word => {
        let count = wordMap[word] || 0;
        count++;
        wordMap[word] = count;
    });
})

stream.on('end', () => {
    let wordRank = [];
    Object.keys(wordMap).forEach(key => {
        wordRank.push({
            count: wordMap[key],
            key,
        });
    });
    wordRank = wordRank.sort((v1, v2) => {
        return v2.count - v1.count;
    });
    console.log('the top 7 vocabulary: ', wordRank.slice(0, 6));
    console.log('vocabulary: ', Object.keys(wordMap).length);
});


