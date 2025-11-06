declare global {
    interface Window {
        cardano?: any;
    }
}

export const connectWallet = async (): Promise<void> => {
    if (window.cardano) {
        // Try Nami first
        if (window.cardano.nami) {
            await window.cardano.nami.enable();
        } else if (window.cardano.eternl) {
            await window.cardano.eternl.enable();
        } else {
            throw new Error('No supported wallet found');
        }
    } else {
        throw new Error('No Cardano wallet found');
    }
};

export const getAddress = async (): Promise<string> => {
    if (window.cardano.nami) {
        return await window.cardano.nami.getChangeAddress();
    } else if (window.cardano.eternl) {
        return await window.cardano.eternl.getChangeAddress();
    } else {
        throw new Error('No supported wallet enabled');
    }
};

export const getBalance = async (): Promise<string> => {
    if (window.cardano.nami) {
        const balance = await window.cardano.nami.getBalance();
        return balance;
    } else if (window.cardano.eternl) {
        const balance = await window.cardano.eternl.getBalance();
        return balance;
    } else {
        throw new Error('No supported wallet enabled');
    }
};